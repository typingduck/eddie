
/*
 * Plain networking support code 
 *
 * Copyright (C) 1999 Ericsson.
 *
 * 
 * Must have:
 *  void init_child()
 *  void close_child(Server * me, int fd)
 *  void init_protocol(int client_fd)
 *  int handle_pre_backend(Server * me, int fd)
 *  int protocol_write(Server * me, int fd, int &error)
 *  int protocol_read(Server * me, int fd, int &error)
 */
#include <unistd.h>
#include <fcntl.h>

#include "backend.h"
#include "server.h"
#include "control.h"
#include "main.h"


/*
 * Name: init_child
 * Purpose: initialise child connection related stuff
 */
void init_child(Server *serv)
{
}

/*
 * Name: close_child
 * Purpose:  protocol dependent closing/cleaning up for a child
 *      client
 */
void close_child(Server *me, int fd)
{
}

/*
 * Name: init_connection()
 * Purpose: initialise a non-SSL connection upon creation
 * Sets non-SSL connections to be non-blocking
 */
void init_connection(Server *me, int client_fd)
{
	/* set client socket non-blocking - we don't want to wait on writes */
	fcntl(client_fd, F_SETFL,
                fcntl(client_fd, F_GETFL, 0) | O_NONBLOCK);

}


/*
 * Name: handle_pre_backend
 * Purpose: handle pre-backend stuff 
 * Returns: 0 on failure, 1 on success
 */
int handle_pre_backend(Server *me, int fd)
{
    me->fd_state[fd] = fs_no_backend;
    return -1;
}

/*
 * Name: protocol_write
 * Purpose: write using a specialised (or not) protocol
 * Returns: 1 on success, 0 on failure.  'retry' is +ve if the failure is
 * transient, and the same operation could be tried again.
 */
int protocol_write(Server *me, int fd, int chars_to_write, int *retry)
{
    *retry = 0;
    return generic_write(fd, me, chars_to_write);
}

/*
 * Name: protocol_read
 * Purpose: read using a specific protocol
 * Returns: characters read, retry is +ve if there's an unimportant error
 */

int protocol_read(Server *me, int fd, int *retry, int start, int length)
{
    int chars_read;

    /* FAIL POINT 5 ?? */

    chars_read = read(fd, &(me->writebuf.buffer[start]), length);
    if (chars_read < 0) 
	*retry = (errno == EINTR);

    else
	*retry = 0;

    return chars_read;
}

