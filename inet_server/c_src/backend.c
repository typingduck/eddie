#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <fcntl.h>
#include <syslog.h>

#include "backend.h"
#include "main.h"

int open_connection(Server *serv, u32 ip, int port, int block, int BE) 
{
    struct sockaddr_in server_addr;
    int fd;

    /* make a socket */
    fd = socket(AF_INET, SOCK_STREAM, 0);
    if (fd < 0)
	return fd;
    /* set up server socket */
    bzero(&server_addr, sizeof(struct sockaddr_in));
    server_addr.sin_family = AF_INET;
    server_addr.sin_addr.s_addr = ip;
    server_addr.sin_port = htons(port);

    /* make socket non-blocking */
    if (!block)
	fcntl(fd, F_SETFL, fcntl(fd, F_GETFL, 0) | O_NONBLOCK);

    /* connect to server */
    if (connect(fd, (struct sockaddr *) &server_addr,
	    sizeof(struct sockaddr_in)) < 0)
    {
	if (errno == EINPROGRESS)
	{
	    serv->fd_state[fd] = fs_pending_connect;
	    serv->writebuf.num_chars[fd] = -1;

            /* non-blocking connect, if it's to a backend
            then put a timeout on the select.
            */
            if (BE)
            {
    //syslog(LOG_INFO, "backend.c (pid %d) INCREMENTING pending connects", getpid()); 
              serv->glob_timeout = &(serv->timeout); 
              serv->total_pending_connects++;
              serv->fd_connect_start[fd] = time(NULL);    
            }  // if (BE)
            else
              serv->fd_connect_start[fd] = 0;    

#ifdef DEBUG
	    printf("fd %d (pid %ld) deferred\n", fd, getpid()); 
#endif
	}
	else
	{
	    syslog(LOG_INFO, "Pid %d trying to connect to %s, port %d: %m",
		    (int) getpid(), inet_ntoa(*((struct in_addr *) &ip)), port);
	    close(fd);
	    return -1;
	}
    }
    else
    {
	serv->fd_state[fd] = fs_connected;
        serv->fd_connect_start[fd] = 0;    
    }  // else

    return fd;
}
