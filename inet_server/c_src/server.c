/*
**
** Server.c
**
** Purpose: provide the main serving functions and establish the
**      initial bound/listen socket for children to use
**      when accepting incoming data.
**
** Author: Rob Rendell.
** Date:   Sep 1999.
**
**
**
**
** TO-DO: set fd_connect_start[cc] = -1; when a connect passes
*/

#include <signal.h>
#include <syslog.h>
#include <fcntl.h>

#include <sys/stat.h>
#include <unistd.h>

#include "param.h"
#include "backend.h"
#include "server.h"
#include "child.h"
#include "main.h"

/*------------------------------------------------------------------*/
/*------------------------------------------------------------------*/
/* global for the alarm signal handler */
Server *SigServ;
/*
int Crap_Loop_Flag = 0;
void sighup_handler(int signo)
{
  Crap_Loop_Flag = 1;
} 
*/
void simple_handler(int signo)
{
  int Ppid = (int)getpid();

  syslog(LOG_INFO, "(pid %d):  Alarm timed out ==> num_connections=(%d);  max_connections=(%d);  busy=(%d);  control_fd=(%d);  control_port=(%d);  buffer=(%d);  total_pending_connects=(%d)", 
    Ppid,
    SigServ->num_connections,
    SigServ->max_connections,
    SigServ->busy,
    SigServ->control_fd,
    SigServ->control_port,
    (int)SigServ->writebuf.buffer,
    SigServ->total_pending_connects );

  //syslog(LOG_INFO, "Current (or Previous) HTTP request (%s)", SigServ->last_http_req);

  if (SigServ->glob_timeout == NULL)    
    syslog(LOG_INFO, "(pid %d):  TIMEOUT glob_timeout=NULL", (int)Ppid);
  else
    syslog(LOG_INFO, "(pid %d):  TIMEOUT glob_timeout=(%d)secs", (int)Ppid, SigServ->glob_timeout->tv_sec);
  
  abort();
}
/*------------------------------------------------------------------*/
/*------------------------------------------------------------------*/



void shutdown_ipc(Server *serv)
{
    int fd;

    for (fd = 0; fd < MAX_FD; fd++)
	if (FD_ISSET(fd, &serv->read_fds) || FD_ISSET(fd, &serv->write_fds))
	{
	    /* shutdown(fd, 2); */
	    close(fd);
	}
    FD_ZERO(&serv->read_fds);
    FD_ZERO(&serv->write_fds);
    FD_ZERO(&serv->except_fds);
    /* shutdown(serv->listen_fd, 2); */
    close(serv->listen_fd);
    if (serv->control_fd > 0)
	close(serv->control_fd);
    if (serv->parent_fd > 0)
	close(serv->parent_fd);
    serv->listen_fd = serv->max_connection_fd = -1;
}

Server *server_new(u32 ip, int port, int listen_queue)
{
    Server *result;
    struct sockaddr_in address;
    struct sockaddr *sa_address;
    int length;
    int pos;
    int option_val;

    result = (Server *) malloc(sizeof(Server));
    if (!result)
	fatal(NULL, "Unable to allocate the memory for the server structure!");
    result->writebuf.buffer = (char *) malloc(buffer_len);
    if (!result->writebuf.buffer)
	fatal(NULL, "Unable to allocate the memory for the data buffer!");
    result->writebuf.buffer_len = buffer_len;
    result->max_connection_fd = 0;
    result->num_connections = 0;
    result->busy = 0;
    FD_ZERO(&result->read_fds);
    FD_ZERO(&result->write_fds);
    FD_ZERO(&result->except_fds);
    result->writebuf.first_busy = -1;
    result->writebuf.last_busy = -1;
    for (pos = 0; pos < MAX_FD; pos++) 
	result->writebuf.num_chars[pos] = -1;
    sa_address = (struct sockaddr *) &address;
    /* create the socket */
    result->listen_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (result->listen_fd < 0)
	fatal(NULL, "Couldn't open server socket: %m\n");
    /* bind to the given port (arbitrary one if port == DONT_CARE) */
    bzero(&address, sizeof(struct sockaddr_in));
    address.sin_family = AF_INET;
    address.sin_addr.s_addr = ip;
    address.sin_port = htons(port);
    option_val = 1;
    setsockopt(result->listen_fd, SOL_SOCKET, SO_REUSEADDR,
	    (char *) &option_val, sizeof(int));
    if (bind(result->listen_fd, sa_address, sizeof(struct sockaddr_in)))
	fatal(result, "Couldn't bind to server socket: %m\n");
    /* Get the port number, if caller didn't state it explicitly */
    if (port == DONT_CARE)
    {
	length = sizeof(struct sockaddr_in);
	if (getsockname(result->listen_fd, sa_address, &length))
	    fatal(result, "Unable to get socket number: %m\n");
	result->port = ntohs(address.sin_port);
    }
    else
	result->port = port;
    /* set the size of the listen queue */
    listen(result->listen_fd, listen_queue);
    /* now that we've opened the (possibly privileged) listen socket,
     * revert to our actual uid, in case we're running suid */
    setuid(getuid());
    setgid(getgid());

    /* set listen socket non-blocking */
    fcntl(result->listen_fd, F_SETFL, 
		fcntl(result->listen_fd, F_GETFL, 0) | O_NONBLOCK);

    for (pos = 0; pos < MAX_FD; pos++) 
	result->fd_partner[pos] = -1;
    result->control_fd = -1;
    result->parent_fd = -1;
    result->max_connections = max_connections;
    result->fd_state[result->listen_fd] = fs_connected;
    result->fd_status[result->listen_fd] = ft_other;

    /* ugly alarm hack to set all ports to zero */
    for (pos = 0; pos < MAX_FD; pos++) 
      result->fd_connection_port[pos] = 0;

    return result;
}

void set_server_read_fd(Server *serv, int fd)
{
    FD_SET(fd, &serv->read_fds);
    if (fd > serv->max_connection_fd)
	serv->max_connection_fd = fd;
}

void reset_server_read_fd(Server *serv, int fd)
{
    FD_CLR(fd, &serv->read_fds);
    /* drop serv->max_connection_fd if it == fd, and fd's bit isn't set in
     * the other fd_sets */
    if (fd == serv->max_connection_fd)
	while (serv->max_connection_fd > 0 &&
		!FD_ISSET(serv->max_connection_fd, &serv->read_fds) &&
		!FD_ISSET(serv->max_connection_fd, &serv->write_fds))
	    serv->max_connection_fd--;
}

void set_server_write_fd(Server *serv, int fd)
{
    FD_SET(fd, &serv->write_fds);
    if (fd > serv->max_connection_fd)
	serv->max_connection_fd = fd;
}

void reset_server_write_fd(Server *serv, int fd)
{
    FD_CLR(fd, &serv->write_fds);
    /* drop serv->max_connection_fd if it == fd, and fd's bit isn't set in
     * the other fd_sets */
    if (fd == serv->max_connection_fd)
	while (serv->max_connection_fd > 0 &&
		!FD_ISSET(serv->max_connection_fd, &serv->read_fds) &&
		!FD_ISSET(serv->max_connection_fd, &serv->write_fds))
	    serv->max_connection_fd--;
}

void change_state(Server *serv, int fd, Fd_State new_state)
{
    serv->fd_state[fd] = new_state;
    if (new_state >= fs_output)
    {
	if (serv->fd_partner[fd] >= 0)
	    reset_server_read_fd(serv, serv->fd_partner[fd]);
	set_server_write_fd(serv, fd);
    }
    else
    {
	if (serv->fd_partner[fd] >= 0)
	    set_server_read_fd(serv, serv->fd_partner[fd]);
	reset_server_write_fd(serv, fd);
    }
}

struct timeval *glob_timeout = NULL;

void set_select_timeout(struct timeval *timeout)
{
    glob_timeout = timeout;
}

/* Magic!  Exact powers of two, modulo 37, map to unique values, up to 1<<31 */
int glob_log2[] = { -1, 0, 1, 26, 2, 23, 27, -1, 3, 16, 24, 30, 28, 11, -1, 13,
		4, 7, 17, -1, 25, 22, 31, 15, 29, 10, 12, 6, -1, 21, 14, 9,
		5, 20, 8, 19, 18 };

void server_main_loop(Server *me, void (*write_fn)(Server *, int),
	void (*read_fn)(Server *, int), void (*except_fn)(Server *, int))
{
    void (*actions[3])(Server *, int);
    fd_set fds[3];
    int num_waiting;
    int fd_type;
    fd_mask *curr_mask, mask;
    unsigned long lsb;
    int base_fd, curr_fd;
    struct sigaction sa;
    //struct sigaction sa2;

    /*------------------------------------------------------*/
    /*------------------------------------------------------*/
    /* alarm handler stuff */
    sa.sa_handler = simple_handler;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;

    if (sigaction(SIGALRM, &sa, NULL))
    {
      syslog(LOG_INFO, "pid %d : Couldn't create the alarm signal handler", (int)getpid());
      exit(1);
    }  // if

    SigServ = me;
    /*------------------------------------------------------*/
    /*------------------------------------------------------*/

    /* SIGHUP handler stuff */
/*
    sa2.sa_handler = sighup_handler;
    sigemptyset(&sa.sa_mask);
    sa2.sa_flags = 0;

    if (sigaction(SIGHUP, &sa2, NULL))
    {
      syslog(LOG_INFO, "pid %d : Couldn't create the sighup signal handler", (int)getpid());
      exit(1);
    }  // if
*/
    /*------------------------------------------------------*/
    /*------------------------------------------------------*/



    actions[0] = write_fn;
    actions[1] = read_fn;
    actions[2] = except_fn;

    /* set the defaults for pending connections and timeout */
    me->glob_timeout = NULL;
    me->total_pending_connects = 0;

    while (1)
    {
	fds[0] = me->write_fds;
	fds[1] = me->read_fds;
	fds[2] = me->except_fds;

        /* reset the me->timeout parameters after each select */
        me->timeout.tv_sec = SELECT_TIMEOUT;
        me->timeout.tv_usec = 0;

        /* flag to remove a backend */
        me->remove_backend = 0;

/*
if (me->glob_timeout == NULL)
syslog(LOG_INFO, "(pid %d) ---> select, pending = %d, timeout = NULL", 
  (int)getpid(), me->total_pending_connects);
else
syslog(LOG_INFO, "(pid %d) ---> select, pending = %d, timeout = %d", 
  (int)getpid(), me->total_pending_connects, me->glob_timeout->tv_sec );
*/

        /* clear the alarm */
        if (debug < 2) alarm(0);

        /* select(...), wait for something to happen */
	num_waiting = select(me->max_connection_fd + 1, &fds[1], &fds[0],
		&fds[2], me->glob_timeout);

        /* clear the alarm */
        if (debug < 2) alarm(alarm_time);

	if (num_waiting < 0)
	{
        /* just interrupted with a signal */
	    if (errno == EINTR) continue;	
	    if (errno == EAGAIN) continue;	
	    /* TODO: potentially, could go through turning off fds in the
	     * fd_sets, trying to isolate the problem fd (on the assumption
	     * that the only error we're likely to get here is EBADF), then
	     * continue servicing all the other current connections.
	     * However, since I believe the only way for a fd to go "bad"
	     * is for us to close it & then select on it, rather than
	     * anything the remote end can do, I can probably get away with
	     * just fatal()ing on an error, on the grounds that "it'll
	     * never happen!" :-/ */
	    //fatal(me, "Failed while selecting on sockets: %m\n");
	}


	else if (num_waiting == 0)
        {
          if (me->total_pending_connects > 0)
          {
            int cc = 0;
            int found_waiting = 0;  // safety parameter, find when counter
                                    // is out of synch

            /* test each fd to see if any have timed out */
            for (cc = 0; cc <= me->max_connection_fd; cc++)
            {
              time_t now = time(NULL);  // get the time now

              if ((me->fd_state[cc] == fs_pending_connect) 
                  && ((now - me->fd_connect_start[cc]) > (SELECT_TIMEOUT-1))
                  && (me->fd_connect_start[cc] > 0))
              {
                found_waiting++;
                //me->fd_connect_start[cc] = 0;
                //me->total_pending_connects--;

                /* if no more connects then reset timeout to NULL */
                //if (me->total_pending_connects == 0)
                //  me->glob_timeout = NULL;

                /* flag to remove a backend */
                me->remove_backend = 1;

                /* run the child_write, it will realise the backend is down */
                (*actions[0])(me, cc);

                //andrew_stop(me, cc);

              }  // if
            }  // for


            // What if total_pending connects is wrong?
            // Check here, if it is then do something about it.
            // This is definitely a kludge!!
            if ((found_waiting == 0) && (me->total_pending_connects > 0)) 
            {
              me->total_pending_connects--; 

              /* if no more connects then reset timeout to NULL */
              if (me->total_pending_connects == 0)
                me->glob_timeout = NULL;

              syslog(LOG_INFO, "(pid %d), server.c has a bad counter ", (int)getpid()); 

            } // if ((found_waiting ...

            continue;

          }  // if (me->total_pending_connects > 0)

          //syslog(LOG_INFO, "(pid %d) RELAY IS DYING NOW", (int)getpid() );

	  return;  // ?? what are the side effects??
        }  // else if (num_waiting == 0)



        //else
        { 
          for (fd_type = 0; num_waiting && fd_type < 3; fd_type++)
	  {
	    if (!actions[fd_type])
		continue;
#if defined(__GLIBC__) && (__GLIBC__ >= 2) && (__GLIBC_MINOR__ > 0)
	    curr_mask = fds[fd_type].__fds_bits;
#else
	    curr_mask = fds[fd_type].fds_bits;
#endif
	    for (base_fd = 0; num_waiting && base_fd <= me->max_connection_fd;
		    curr_mask++, base_fd += NFDBITS)
	    {
		mask = *curr_mask;
		while (mask)
		{
		    num_waiting--;
		    /* get the least significant set bit in mask */
		    lsb = mask&-mask;
		    /* clear it in mask */
		    mask ^= lsb;
		    /* use log2 magic to get its index */
		    curr_fd = base_fd + glob_log2[lsb%37];
		    /* call the function to act on the socket */
                    if (curr_fd == -1)
                    {
	                syslog(LOG_INFO, "curr_fd == -1 this is BAD");
                    }
		    if (num_waiting >= 0) 
		      (*actions[fd_type])(me, curr_fd);
		    else {
			struct stat sbuf;
			if (fstat(curr_fd, &sbuf) < 0) {
			    syslog(LOG_INFO, "pid %d fd %d bad: %m", 
				(int)getpid(), curr_fd);
			    if (me->fd_status[curr_fd] == ft_client) {
				syslog(LOG_INFO, "client deleted");
				delete_client(me, curr_fd);
			    }
			    else {
				fatal(me, 
				    "non-client bad fd, state %d status %d",
				    me->fd_state[curr_fd], 
				    me->fd_status[curr_fd]);
			    }
			}

		    }
		}  // while (mask)
	    }  // for
	}  // for
      }  // else
      if (num_waiting < 0) 
	    syslog(LOG_INFO, "Failed while selecting on sockets: %m\n");
    }  // while (1)
}  // server_main_loop

/* actually writes the output in serv's writebuf destined for fd to fd.
 * Returns 1 if all is well, and 0 if an error occured.
*/
int generic_write(int fd, Server *serv, int chars_to_write)
{
    int chars_written;

    if (serv->fd_state[fd] < fs_output)
	fatal(serv, "child attempted to generic_write non-output\n");
    if (chars_to_write <= 0 || chars_to_write > serv->writebuf.num_chars[fd])
	chars_to_write = serv->writebuf.num_chars[fd];
    chars_written = write(fd, &(serv->writebuf.buffer[serv->writebuf.pos[fd]]),
	    chars_to_write);
#ifdef DEBUG
    printf("child wrote %d chars to fd %d\n", chars_written, fd); 
#endif
    if (chars_written < 0)
    {
    /* just interrupted with a signal - try again later */
	if (errno == EINTR) return 1;	
	if (errno == EAGAIN) return 1;	

        #ifdef DEBUG
	syslog(LOG_INFO, "child %d failed writing to fd %d (discarding "
		"buffered output): %m\n", (int) getpid(), fd);
        #else

        if (serv->fd_status[fd] == ft_backend)
	  syslog(LOG_INFO, "child %d failed writing to a web server (fd %d)",
	            (int)getpid(), fd);

        else if (serv->fd_state[fd] == fs_pending_connect)
	  syslog(LOG_INFO, "child %d a pending connect has gone (fd %d)",
	            (int) getpid(), fd);
        #endif
    }
    else if (chars_written > 0)
    {
	serv->writebuf.pos[fd] += chars_written;
	serv->writebuf.num_chars[fd] -= chars_written;
    }
    if (chars_written < 0 || serv->writebuf.num_chars[fd] == 0)
    {
	delete_data(serv, fd);
	if (chars_written < 0)
	    return 0;
    }
    return 1;
}

/* set *start and *end to point to the start and end, respectively, of the
 * largest chunk of _unallocated_ space in serv's writebuf. */
int locate_space(Server *serv, int *start, int *end)
{
    /* locate the free space in the write buffer */
    if (serv->writebuf.first_busy == -1)
    {
	/* buffer is completely empty */
	/* printf("buffer is empty\n"); */
	*start = 0;
	*end = serv->writebuf.buffer_len;
    }
    else
    {
	*start = serv->writebuf.pos[serv->writebuf.last_busy] +
		serv->writebuf.num_chars[serv->writebuf.last_busy];
	*end = serv->writebuf.pos[serv->writebuf.first_busy];
	if (*end < *start)
	{
	    /* allocated chunks don't wrap around the buffer (and
	     * therefore, the free space does).  We can either write to
	     * the space from *start to serv->writebuf.buffer_len, or from
	     * 0 to *end. */
	    /* printf("busy chunk 'contiguous'.  There are %d bytes from *start to serv->writebuf.buffer_len, and %d from 0 to the first busy chunk.\n", serv->writebuf.buffer_len - *start, *end); */
	    if (*start > serv->writebuf.buffer_len - buffer_reserve &&
		    serv->writebuf.buffer_len - *start < *end)
	    {
		/* we're better off returning to the start of the array */
		*start = 0;
		/* printf("\treturned to start of array\n"); */
	    }
	    else
	    {
		*end = serv->writebuf.buffer_len;
		/* printf("\tstayed at the end of the array\n"); */
	    }
	}
	if (*end == *start) {
		grow_buffer(serv, start, end, 1, 0);
	}
    }
    return *end - *start;
}

/* adds data to serv's writebuf, stored for 'fd'.  If buffer is NULL,
 * then 'length' chars have already been written into the buffer, at
 * position 'pos'.  Otherwise, calculate the position to write to, and
 * copy 'length' characters from 'buffer' into serv's writebuf.
 *
 * Returns 1 on success, or 0 if something didn't work
 *
 * WARNING: this function can call grow_buffer, which reallocates
 * writebuf.buffer and moves around all the writebuf.pos[]s - be aware that
 * after calling add_data(), any values depending on that data in local
 * variables will need to be re-acquired.
 */
int add_data(Server *serv, int fd, char *buffer, int pos, int length,
	Fd_State data_type)
{
    int free_start, free_end;
    int start;
    int space;
    char *old_buffer;

    space = locate_space(serv, &free_start, &free_end);
    if (fd == -1)
    {
	syslog(LOG_INFO, "proc %d, attempted to add_data to fd -1",
		(int) getpid());
	return 0;
    }
    if (serv->writebuf.num_chars[fd] != -1)
    {
	/* There's data for this fd already in the buffer.  Ensure that
	 * what we're adding is both the same type, and being added
	 * directly after it */
	if (serv->fd_state[fd] != data_type)
	{
	    syslog(LOG_INFO, "proc %d, fd %d added different type of data "
		    "(num_chars = %d)!\n", (int) getpid(), fd,
		    serv->writebuf.num_chars[fd]);
	    return 0;
	}
	else if (!buffer && (serv->writebuf.last_busy != fd
		/* || pos != free_start */))
	{
	    syslog(LOG_INFO, "proc %d, fd %d added second lot of data out "
		    "of sequence (num_chars = %d)!\n", (int) getpid(), fd,
		    serv->writebuf.num_chars[fd]);
	    return 0;
	}
    }
    else
	serv->fd_state[fd] = data_type;
    if (data_type >= fs_output)
    {
	if (serv->fd_partner[fd] >= 0)
	    reset_server_read_fd(serv, serv->fd_partner[fd]);
	set_server_write_fd(serv, fd);
    }
    if (buffer)
    {
	if (space < length)
	    grow_buffer(serv, &free_start, &free_end, 1, 0);
	bcopy(buffer, &serv->writebuf.buffer[free_start], length);
	start = free_start;
    }
    else
	start = pos;
    if (serv->writebuf.num_chars[fd] != -1)
	serv->writebuf.num_chars[fd] += length;
    else
    {
	if (start < free_start || start >= free_end ||
		start - free_start >= free_end - start)
	{
	    /* ok - we don't have the simple "just append it" case.  Copy
	     * the data to the start of our free space, and append anyway!
	     */
	    if (free_end - free_start < length)
	    {
		/* oops - not enough space.  Grow the buffer, and then copy
		 * the data across from the old buffer */
		old_buffer = grow_buffer(serv, &free_start, &free_end, 0, 0);
		bcopy(&old_buffer[start],
		    &(serv->writebuf.buffer[free_start]), length);
		free(old_buffer);
	    }
	    else
		bcopy(&(serv->writebuf.buffer[start]),
			&(serv->writebuf.buffer[free_start]), length);
	    start = free_start;
	}
	serv->writebuf.num_chars[fd] = length;
	serv->writebuf.pos[fd] = start;
	serv->writebuf.next_busy[fd] = -1;
	serv->writebuf.prev_busy[fd] = serv->writebuf.last_busy;
	if (serv->writebuf.first_busy == -1)
	    serv->writebuf.first_busy = fd;
	if (serv->writebuf.last_busy != -1)
	    serv->writebuf.next_busy[serv->writebuf.last_busy] = fd;
	serv->writebuf.last_busy = fd;
    }
    return 1;
}

/* Reassign the first 'length' chars queued for from_fd so that they're now
 * queued for to_fd.
 * If to_fd has no data currently, simply change the pointers, 'moving' the
 * chars in-place.  If, however, to_fd has stuff queued for it already, we
 * have to shift around the data so it's at the end and all.  Therefore:
 * WARNING: this function can call grow_buffer, which reallocates
 * writebuf.buffer and moves around all the writebuf.pos[]s - be aware that
 * after calling append_data(), any values depending on that data in local
 * variables will need to be re-acquired.
 */
void move_data(Server *serv, int from_fd, int to_fd, int length)
{
    int free_start, free_end;

    if (length < 0 || length > serv->writebuf.num_chars[from_fd])
	length = serv->writebuf.num_chars[from_fd];
    if (serv->writebuf.num_chars[to_fd] != -1)
    {
        append_data(serv, to_fd, &free_start, &free_end, serv->fd_state[to_fd]);
        if (free_end - free_start < length)
          /* There's not enough space left.  Grow the buffer. */
          grow_buffer(serv, &free_start, &free_end, 1, length);

	if (serv->writebuf.last_busy != to_fd)
	{
	    syslog(LOG_INFO, "proc %d, fd %d move_data()ed out of sequence "
		    "(num_chars = %d)!\n", (int) getpid(), to_fd,
		    serv->writebuf.num_chars[to_fd]);
	    return;
	}
	else if (serv->fd_state[to_fd] != serv->fd_state[from_fd])
	{
	    syslog(LOG_INFO, "proc %d, fd %d move_data()ed different type "
		    "of data (num_chars = %d)!\n", (int) getpid(), to_fd,
		    serv->writebuf.num_chars[to_fd]);
	    return;
	}
	bcopy(&serv->writebuf.buffer[serv->writebuf.pos[from_fd]],
		&serv->writebuf.buffer[free_start], length);
	serv->writebuf.num_chars[to_fd] += length;
    }
    else
    {
	serv->writebuf.pos[to_fd] = serv->writebuf.pos[from_fd];
	serv->writebuf.num_chars[to_fd] = length;
	/* insert to_fd before from_fd */
	serv->writebuf.next_busy[to_fd] = from_fd;
	serv->writebuf.prev_busy[to_fd] = serv->writebuf.prev_busy[from_fd];
	serv->writebuf.prev_busy[from_fd] = to_fd;
	if (serv->writebuf.first_busy == from_fd)
	    serv->writebuf.first_busy = to_fd;
	else
	    serv->writebuf.next_busy[serv->writebuf.prev_busy[to_fd]] = to_fd;
	serv->fd_state[to_fd] = serv->fd_state[from_fd];
    }
    /* shrink or delete the data in from_fd */
    if (serv->writebuf.num_chars[from_fd] == length)
	delete_data(serv, from_fd);
    else
    {
	serv->writebuf.pos[from_fd] += length;
	serv->writebuf.num_chars[from_fd] -= length;
    }
    /* manipulate the fd_sets if we're shifting output */
    if (serv->fd_state[to_fd] >= fs_output)
    {
	if (serv->fd_partner[to_fd] != -1)
	    reset_server_read_fd(serv, serv->fd_partner[to_fd]);
	set_server_write_fd(serv, to_fd);
    }
}

void delete_data(Server *serv, int fd)
{
    if (serv->writebuf.num_chars[fd] >= 0)
    {
	if (serv->fd_state[fd] >= fs_output)
	{
	    if (serv->fd_partner[fd] >= 0)
		set_server_read_fd(serv, serv->fd_partner[fd]);
	    reset_server_write_fd(serv, fd);
	}
	serv->writebuf.num_chars[fd] = -1;
	if (serv->writebuf.first_busy == fd)
	    serv->writebuf.first_busy = serv->writebuf.next_busy[fd];
	else
	    serv->writebuf.next_busy[serv->writebuf.prev_busy[fd]] =
		    serv->writebuf.next_busy[fd];
	if (serv->writebuf.last_busy == fd)
	    serv->writebuf.last_busy = serv->writebuf.prev_busy[fd];
	else
	    serv->writebuf.prev_busy[serv->writebuf.next_busy[fd]] =
		    serv->writebuf.prev_busy[fd];
    }
}

/* Locate free space, a la locate_space(), but in addition, if fd has
 * data pending, move it to the start of the free space so it may be
 * appended to
 * WARNING: this function can call grow_buffer, which reallocates
 * writebuf.buffer and moves around all the writebuf.pos[]s - be aware that
 * after calling append_data(), any values depending on that data in local
 * variables will need to be re-acquired.
 */
void append_data(Server *serv, int fd, int *start, int *end, Fd_State data_type)
{
    int length;
    int old_pos;
    int next_fd;

    locate_space(serv, start, end);
    if (serv->writebuf.num_chars[fd] > 0 && 
	    *start != serv->writebuf.pos[fd] + serv->writebuf.num_chars[fd])
    {
	if (serv->fd_state[fd] != data_type)
	    fatal(serv, "attempt to append_data of different type\n");
	if (*end - *start < serv->writebuf.num_chars[fd] + buffer_reserve)
	    grow_buffer(serv, start, end, 1, 0);
	old_pos = serv->writebuf.pos[fd];
	length = serv->writebuf.num_chars[fd];
	next_fd = serv->writebuf.next_busy[fd];
	/* copy existing data to the end */
	bcopy(&serv->writebuf.buffer[old_pos], &serv->writebuf.buffer[*start],
		length);
	/* delete old entry */
	delete_data(serv, fd);
	/* if we've just created an easy-to-fill hole, compact our buffer */
	if (*start > old_pos)
	{
	    bcopy(&serv->writebuf.buffer[old_pos + length],
		    &serv->writebuf.buffer[old_pos], *start - old_pos + length);
	    for (; next_fd != -1; next_fd = serv->writebuf.next_busy[next_fd])
		serv->writebuf.pos[next_fd] -= length;
	    *start -= length;
	}
	/* create new entry at the end */
	serv->writebuf.pos[fd] = *start;
	serv->writebuf.num_chars[fd] = length;
	serv->writebuf.next_busy[fd] = -1;
	serv->writebuf.prev_busy[fd] = serv->writebuf.last_busy;
	if (serv->writebuf.last_busy != -1)
	    serv->writebuf.next_busy[serv->writebuf.last_busy] = fd;
	serv->writebuf.last_busy = fd;
	serv->fd_state[fd] = data_type;
	*start += length;
	if (serv->fd_state[fd] >= fs_output)
	{
	    if (serv->fd_partner[fd] >= 0)
		reset_server_read_fd(serv, serv->fd_partner[fd]);
	    set_server_write_fd(serv, fd);
	}
    }
}

/* We've run out of space in our buffer.  Optionally grow it by a factor of
 * buffer_grow_rate, and compact all the current data to the start.  This is
 * quite expensive, but is better than bombing out!  (and only happens
 * occasionally)
 * If 'we_free' is set, we free the old buffer, and return NULL.
 * Otherwise, we'll return the old buffer, which must be freed by the
 * calling function.
 * If 'opt_len' is non-zero, this specifies the amount of free space that 
 * grow_buf must guarantee to be available. If it is zero, then grow_buffer
 * will guarantee buffer_reserve (-r option) by default.
 */

char *grow_buffer(Server *serv, int *free_start, int *free_end, 
	int we_free, int opt_len)
{
    int curr_fd;
    int actual_length, needed_len;
    char *new_buffer;
    int new_buffer_len;
    int pos;
    char *result;

    /* check if we actually need to grow the buffer, or can just compact it */
    for (curr_fd = serv->writebuf.first_busy, actual_length = 0; curr_fd != -1;
	    curr_fd = serv->writebuf.next_busy[curr_fd])
	actual_length += serv->writebuf.num_chars[curr_fd];
    needed_len = (opt_len > buffer_reserve) ? opt_len : buffer_reserve;
    curr_fd = serv->writebuf.first_busy;
    pos = 0;
    if (actual_length > serv->writebuf.buffer_len - needed_len)
    {
	new_buffer_len = serv->writebuf.buffer_len*buffer_grow_rate;
	/* make sure we gurantee needed_len */
	new_buffer_len = (new_buffer_len > 
		actual_length+needed_len+buffer_reserve) ? new_buffer_len :
		(actual_length+needed_len+buffer_reserve);
	syslog(LOG_INFO, "child %d grew its %d-byte writebuffer to %d bytes "
		"(if this happens excessively, think about increasing "
		"buffer_len or buffer_grow_rate with the relay's command line switches)\n",
		(int) getpid(), serv->writebuf.buffer_len, new_buffer_len);
	new_buffer = (char *) malloc(new_buffer_len);
    }
    else
    {
	/* we don't need to grow the buffer, just use the space more
	 * efficiently - compact the data to the start of buffer */
	new_buffer_len = serv->writebuf.buffer_len;
	if (we_free && serv->writebuf.pos[serv->writebuf.last_busy] +
		serv->writebuf.num_chars[serv->writebuf.last_busy] >
		serv->writebuf.pos[serv->writebuf.first_busy])
	{
	    /* We're responsible for freeing the old buffer (or not), and
	     * the data doesn't wrap around the end of the buffer - do the
	     * compaction in-place */
	    new_buffer = serv->writebuf.buffer;
	    /* skip any initial entries that are already compact at the
	     * start of the buffer */
	    for (; curr_fd != -1 && serv->writebuf.pos[curr_fd] == pos;
		    curr_fd = serv->writebuf.next_busy[curr_fd])
		pos += serv->writebuf.num_chars[curr_fd];

            if (debug)
	      syslog(LOG_INFO, "child %d buffer shrunk to %d-byte from %d-byte",
		    (int) getpid(), new_buffer_len, actual_length); 

	}  // if (we_free...
	else
	{
	    /* actually allocate a new buffer */
	    new_buffer = (char *) malloc(new_buffer_len);

            if (debug)
	      syslog(LOG_INFO, "child %d buffer increased to %d-byte from %d-byte",
		    (int) getpid(), new_buffer_len, actual_length);
	}
    }
    if (!new_buffer)
	fatal(serv, "Overflowed buffer, and was unable to alloc more space "
		"(growing from %d bytes to %d bytes)",
		serv->writebuf.buffer_len, new_buffer_len);
    /* compact the old data into our new buffer */
    for (; curr_fd != -1; curr_fd = serv->writebuf.next_busy[curr_fd])
    {
	bcopy(&serv->writebuf.buffer[serv->writebuf.pos[curr_fd]],
		&new_buffer[pos], serv->writebuf.num_chars[curr_fd]);
	serv->writebuf.pos[curr_fd] = pos;
	pos += serv->writebuf.num_chars[curr_fd];
    }
    if (we_free)
    {
	if (new_buffer != serv->writebuf.buffer)
	    free(serv->writebuf.buffer);
	result = NULL;
    }
    else
	result = serv->writebuf.buffer;
    serv->writebuf.buffer = new_buffer;
    serv->writebuf.buffer_len = new_buffer_len;
    *free_start = pos;
    *free_end = new_buffer_len;
    return result;
}

/* 
 * if a partial amount of data had previously been read in, and then some
 * more is appended, update chars_read and start to have values as if the
 * data has all just been read in.  Dispose of the old packet's record 
 */
void backdate_read(Server *serv, int fd, int *chars_read, int *start)
{
    if (serv->writebuf.num_chars[fd] > 0 && serv->writebuf.last_busy == fd)
    {
	*chars_read += serv->writebuf.num_chars[fd];
	*start -= serv->writebuf.num_chars[fd];
	delete_data(serv, fd);
    }
}
