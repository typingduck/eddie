#include <signal.h>
#include <sys/wait.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <assert.h>
#include <syslog.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include "main.h"
#include "parent.h"
#include "control.h"
#include "child.h"
#include "backend.h"
#include "param.h"

Server *glob_server;
Children glob_child_dat;

int create_child(Server *serv)
{
    int pipe_fd[2];
    int pid;
    int child;
    int num_closed;

    if (socketpair(AF_UNIX, SOCK_STREAM, 0, pipe_fd) < 0)
    {
        syslog(LOG_INFO, "create_child failed to socketpair: %m\n");
	return 0;
    }
#ifdef DEBUG
    fprintf(stderr,"created socketpair: %d %d\n", pipe_fd[0], pipe_fd[1]); 
#endif
    for (child = 0; child < max_children &&
	    glob_child_dat.children[child] != -1; child++)
	;
    assert(child < max_children); /* should never happen */

#ifdef DEBUG
    if (child < min_children)
	fprintf(stderr,"\ncreating permanent child %d\n", child);
    else
	fprintf(stderr,"\ncreating transient child %d\n", child);
#endif

    pid = fork();
    if (pid == 0)
    {
	/* I'm the child. */
	/* Zap all the unneeded descriptors inherited from the parent */
	close(pipe_fd[1]);
	for (child = 0, num_closed = 0;
		num_closed < glob_child_dat.num_children; child++)
	{
	    if (glob_child_dat.child_pipe[child] != -1)
	    {
		close(glob_child_dat.child_pipe[child]);
		glob_child_dat.child_pipe[child] = -1;
		num_closed++;
	    }
	}
	child_main(serv, pipe_fd[0], (child >= min_children));
	/* child_main never returns */
    }
    else if (pid > 0)
    {
	/* I'm the parent. */
	/* printf(".");
	fflush(stdout); */
	close(pipe_fd[0]);
#ifdef DEBUG
	fprintf(stderr,"closing fd %d\n", pipe_fd[0]); 
#endif
	serv->fd_partner[pipe_fd[1]] = pipe_fd[1];
	set_server_read_fd(serv, pipe_fd[1]);
	glob_child_dat.children[child] = pid;
	glob_child_dat.child_pipe[child] = pipe_fd[1];
	glob_child_dat.fd_to_child[pipe_fd[1]] = child;
	glob_child_dat.child_busy[child] = 0;
#ifdef DEBUG
	fprintf(stderr," %d\n", child); 
#endif
	glob_child_dat.num_children++;
    }
    else
    {
	syslog(LOG_INFO, "failed to fork child: %m\n");
#ifdef DEBUG
    fprintf(stderr,"closing both fds (%d & %d)\n", pipe_fd[0], pipe_fd[1]); 
#endif
	close(pipe_fd[0]);
	close(pipe_fd[1]);
	return 0;
    }
    return 1;
}

void delete_child(Server *serv, int child)
{
    int fd = glob_child_dat.child_pipe[child];

#ifdef DEBUG
    fprintf(stderr,"deleting child %d (pos %d), which uses fd %d (glob_child_dat.curr_child = %d)\n", glob_child_dat.children[child], child, fd, glob_child_dat.curr_child); 
#endif
    /* shutdown(fd, 2); */
    close(fd);
#ifdef DEBUG
    fprintf(stderr,"closing fd %d\n", fd); 
#endif
    serv->fd_partner[fd] = -1;
    glob_child_dat.child_pipe[child] = -1;
    glob_child_dat.children[child] = -1;
    delete_data(serv, fd);
    reset_server_read_fd(serv, fd);
    reset_server_write_fd(serv, fd);
    FD_CLR(fd, &serv->except_fds);
    glob_child_dat.num_children--;
    if (glob_child_dat.curr_child != -2)
    {
	/* if that drops us below our minimum, start some new children */
	while (glob_child_dat.num_children < min_children && create_child(serv))
	    ;
    }
    /* we've finished, unless the child that died was the one that
	* is currently "it", or unless no children are "it" */
    if (glob_child_dat.curr_child > -1 && child != glob_child_dat.curr_child)
	return;
    /* try to find a new child to listen on the socket. */
    for (child = 0; child < max_children; child++)
	if (glob_child_dat.children[child] >= 0 &&
		!glob_child_dat.child_busy[child] &&
		serv->writebuf.num_chars[glob_child_dat.child_pipe[child]] < 0)
	{
	  glob_child_dat.curr_child = child;
          if (debug)
          {
	    syslog(LOG_INFO,"child died, passing the ball to child %d (# %d)\n", 
                glob_child_dat.children[glob_child_dat.curr_child], 
                glob_child_dat.curr_child); 
          }  // if (debug)

	  add_data(serv,
	    glob_child_dat.child_pipe[glob_child_dat.curr_child],
	    "x", 0, 1, fs_output);
	  return;
	}  // if (glob...
}  // delete_child


#define MAX_WAIT 1024


void terminate(int sig_num)
{
    int count = 0;
    int child;
    int status;

    fprintf(stderr,"\nparent interrupted\n");
    fprintf(stderr,"killing children: ");
    fflush(stdout);
    glob_child_dat.curr_child = -2;
    for (child = 0; child < max_children; child++)
	if (glob_child_dat.children[child] > 0)
	{
	    fprintf(stderr,"%d ", glob_child_dat.children[child]);
	    kill(glob_child_dat.children[child], SIGTERM);
	}
    fprintf(stderr,"\n");
    fflush(stdout);

    /* wait for all children to terminate */
    while ((waitpid(0, &status, WNOHANG) > 0) && (count++ < MAX_WAIT))
        usleep(1)
	;	

    /* BRUTAL termination in case signals have gone missing */
    while (waitpid(0, &status, WNOHANG) > 0)
    {
        for (child = 0; child < max_children; child++)
	if (glob_child_dat.children[child] > 0)
	{
	    fprintf(stderr,"sigkill %d ", glob_child_dat.children[child]);
	    kill(glob_child_dat.children[child], SIGKILL);
	}
    }

    shutdown_ipc(glob_server);
    syslog(LOG_INFO, "Parent interrupted - children shutdown.\n");
    closelog();
    exit(0);
}

void parent_read(Server *serv, int fd)
{
    static int num_busy = 0;

    char buffer[10];
    int chars_read = -1;
    int start;
    int error;
    int pos;

    if (glob_child_dat.children[glob_child_dat.fd_to_child[fd]] == -2)
    {
	delete_child(serv, glob_child_dat.fd_to_child[fd]);
	return;
    }
    else if (glob_child_dat.curr_child == -2)
	return;
    /* brutal debugging */
    if (fd == -1) terminate(0);

    /* FAIL POINT 4 ?? */

    chars_read = read(fd, buffer, 10);
    if (chars_read <= 0)
    {
	if (chars_read < 0)
	{
		/* just ignore read being interrupted by signals */
	    if (errno == EINTR) return;
	    if (errno == EAGAIN) return;
	    syslog(LOG_INFO, "parent failed to read from a child: %m\n");
	}
	/* shutdown if stdin closes */
	if (fd == STDIN_FILENO)
        {
            syslog(LOG_INFO, "Stdin (communication with Erlang) process has failed.\n");
	    terminate(0);
        }
	delete_child(serv, glob_child_dat.fd_to_child[fd]);
	return;
    }
    for (pos = 0; pos < chars_read; pos++)
    {
	switch (buffer[pos])
	{
	case 'f':
	    /* a child has some free capacity... */
#ifdef DEBUG
	    fprintf(stderr,"child %d (# %d) no longer maxed out\n", glob_child_dat.children[glob_child_dat.fd_to_child[fd]], glob_child_dat.fd_to_child[fd]); 
#endif
	    num_busy--;
	    glob_child_dat.child_busy[glob_child_dat.fd_to_child[fd]] = 0;
	    if (glob_child_dat.curr_child != -1)
		/* ..., we're not maxed out, so that's all */
		continue;
	    /* otherwise, make it active! */
#ifdef DEBUG
	    fprintf(stderr,"\tmaking it immediately active\n"); 
#endif
	    glob_child_dat.curr_child = glob_child_dat.fd_to_child[fd] - 1;
	    break;
	case '1':
#ifdef DEBUG
	    fprintf(stderr,"parent received: child %d (# %d) reached capacity\n", glob_child_dat.children[glob_child_dat.fd_to_child[fd]], glob_child_dat.fd_to_child[fd]); 
#endif
	    num_busy++;
	    glob_child_dat.child_busy[glob_child_dat.fd_to_child[fd]] = 1;
	    if (num_busy + min_children >= glob_child_dat.num_children)
	    {
		if (glob_child_dat.num_children < max_children)
		{
#ifdef DEBUG
		    fprintf(stderr,"creating new child\n"); 
#endif
		    if (!create_child(serv) &&
			    num_busy == glob_child_dat.num_children)
		    {
			/* printf("actually, failed to create new child\n"); */
			glob_child_dat.curr_child = -1;
			continue;
		    }
		}
		else if (num_busy == glob_child_dat.num_children)
		{
		    /* printf("\tcan't spawn a new child - have to wait\n"); */
		    glob_child_dat.curr_child = -1;
		    continue;
		}
	    }
	    break;
	default:
	    syslog(LOG_INFO, "parent received invalid message from child "
		    "pid %d (fd %d): %c (%d)\n",
		    glob_child_dat.children[glob_child_dat.fd_to_child[fd]],
		    fd, buffer[pos], (unsigned char) buffer[pos]);
	    continue;
	}
	start = glob_child_dat.curr_child;
	do
	{
	    if (start < 0 && start != glob_child_dat.curr_child)
		start = 0;
	    if (++glob_child_dat.curr_child >= max_children)
		glob_child_dat.curr_child = 0;
	    if (glob_child_dat.children[glob_child_dat.curr_child] >= 0 &&
		    !glob_child_dat.child_busy[glob_child_dat.curr_child] &&
		    serv->writebuf.num_chars[
		    glob_child_dat.child_pipe[glob_child_dat.curr_child]] < 0)
	    {
              if (debug)
		syslog(LOG_INFO, "passing the ball to child %d (# %d)\n", 
                  glob_child_dat.children[glob_child_dat.curr_child], 
                  glob_child_dat.curr_child); 

		error = !add_data(serv,
			glob_child_dat.child_pipe[glob_child_dat.curr_child],
			"x", 0, 1, fs_output);
		/* if (error) printf("failed to add_data\n"); */
	    }
	    else
		error = 1;
	} while (error && glob_child_dat.curr_child != start);
	if (error)
	{
#ifdef DEBUG
	    syslog(LOG_INFO,"Failed to pass the ball to any process.\n"); 
#endif
	    glob_child_dat.curr_child = -1;
	}
    }
}

void parent_write(Server *serv, int fd)
{
    if (glob_child_dat.children[glob_child_dat.fd_to_child[fd]] == -2)
	delete_child(serv, glob_child_dat.fd_to_child[fd]);
    else
	generic_write(fd, serv, -1);
}

void child_signal(int sig_num)
{
    int id;
    int status;
    int child;

    /* for some flavours of unix, re-install the signal handler */
   /* signal(SIGCHLD, child_signal); */
    assert(sig_num == SIGCHLD);
    /* childen exited - wait on them */
    for (id = waitpid(0, &status, WNOHANG); id > 0;
	    id = waitpid(0, &status, WNOHANG))
    {
	for (child = 0; child < max_children &&
		id != glob_child_dat.children[child]; child++)
	    ;
#ifdef DEBUG
	fprintf(stderr,"parent received: child pid %d (pos %d) died\n", id, child); 
#endif
	if (child < max_children)
	{
	    if (glob_child_dat.child_busy[child])
		syslog(LOG_INFO, "Warning - busy child %d (pid %d) died\n",
			child, glob_child_dat.children[child]);
	    /* slate the child to be deleted */
	    glob_child_dat.children[child] = -2;
#ifdef DEBUG
	    fprintf(stderr,"\tpos = %d\n", child); 
#endif
	}
    }
}

void parent_main_loop(int control_port, unsigned long listen_ip,
	int listen_port)
{
    Server *serv;
    int child;
    int control_fd;

    /* specify port and size of the listen queue we're going to use */
    serv = server_new(listen_ip, listen_port, 1024); 
    if (listen_port == DONT_CARE)
	fprintf(stderr,"Started accepting connections on port %d\n", serv->port);
    glob_server = serv;
    serv->control_port = control_port;
    /* confirm that the control port is valid, since if it isn't, we'll
     * have a rather nasty fork-bomb behaviour - children will suicide on
     * startup because they can't connect to the control port, the parent
     * will notice that there are fewer children than min_children, and
     * fork more. */
    control_fd = open_connection(serv,inet_addr(control_ip),control_port,1,0);
    if (control_fd < 0)
	fatal(serv, "parent unable to open control port: %m\n");
    else
	close(control_fd);
#ifdef DEBUG
    fprintf(stderr,"parent pid = %ld\n", getpid());
#endif
    glob_child_dat.num_children = 0;
    for (child = 0; child < max_children; child++)
	glob_child_dat.children[child] = -1;
    for (child = 0; child < min_children; child++)
	(void) create_child(serv);
    signal(SIGCHLD, child_signal);
    signal(SIGPIPE, SIG_IGN);
    glob_child_dat.curr_child = 0;
    /* listen to stdin, so we know when erlang stops */
    set_server_read_fd(serv, STDIN_FILENO);

    /* tell the first child to start handling connections */
    if (debug)
      syslog(LOG_INFO,"passing the ball to child %d (# %d)\n", 
        glob_child_dat.children[glob_child_dat.curr_child], 
        glob_child_dat.curr_child); 

    add_data(serv, glob_child_dat.child_pipe[glob_child_dat.curr_child],
	    "x", 0, 1, fs_output);

    server_main_loop(serv, parent_write, parent_read, NULL);
}
