/*
The contents of this file are subject to the Erlang Public License,
Version 1.0, (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at
http://www.eddieware.org/EPL

Software distributed under the License is distributed on an "AS IS"
basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
the License for the specific language governing rights and limitations
under the License.

The Original Code is Eddie-0.83b1.

The Initial Developer of the Original Code is Ericsson Telecom
AB. Portions created by Ericsson are Copyright (C), 1998, Ericsson
Telecom AB. All Rights Reserved.

Contributor(s): ______________________________________.
*/

/*
 * fdsrv_drv.c
 *
 * Author: Sebastian Strollo <seb@erix.ericsson.se>
 *
 * Program that passes open file descriptors between processes using
 * AF_UNIX stream sockets, as described in Stevens UNIX Network programming.
 *
 * This is an erlang driver which receives the file descriptors created
 * by fdsrv. (It can also be compiled with -DTEST, in which case it will
 * be a standalone test program.)
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/uio.h>
#include <sys/un.h>
#include "config.h"
#include "driver.h"		/* Erlang driver definitions */

/* forward declarations */
static long start();
static int stop();
static int fd_is_ready();
static void recv_fd();

/* driver global variables */
static int instance = -1;
static int  fdsrv;

/* driver structure entry */
#ifdef DYNAMIC_DRIVER
static
#endif
struct driver_entry fdsrv_drv_entry = {
    null_func,
    start,
    stop,
    null_func,
    fd_is_ready,
    null_func,
    "fdsrv_drv"
};

#ifdef DYNAMIC_DRIVER
DriverEntry *driver_init(handle)
    void *handle;
{
    fdsrv_drv_entry.finish = null_func;
    fdsrv_drv_entry.handle = handle;
    return &fdsrv_drv_entry;
}
#endif

/*
 * Called when erlang side does "open_port()".
 */
static long start(port, buf)
    long port;
    char *buf;
{
    int len;
    char *path;
    struct sockaddr_un addr;

    if (instance != -1)		/* only allow one copy at the time */
	return -1;
    if (strlen(buf) > (sizeof(addr.sun_path) + 1))
	return -1;
    instance = port;

    /* Figure out the path to the named socket */
    if ((path = strrchr(buf, (int)' ')) == NULL)
	path = buf;
    else
	path++;

    fdsrv = socket(AF_UNIX, SOCK_STREAM, 0);
    bzero((char *)&addr, sizeof(addr));
    addr.sun_family = AF_UNIX;
    strcpy(addr.sun_path, path);
    len = strlen(path) + sizeof(addr.sun_family) + 1;

    if (connect(fdsrv, (struct sockaddr *)&addr, len) < 0) {
	perror("connect");
	instance = -1;
	return -1;
    }

    /* Have Erlang select on fdsrv */
    driver_select(port, fdsrv, DO_READ, 1);

    return port;
}

/*
 * Called when select triggers. Which is when there is an incomming fd.
 */
static int fd_is_ready(port, readyfd)
    int port;
    int readyfd;
{
    int received_fd;
    int len;
    char buf[256];
	
    if (port != instance)
	return -1;
    if (readyfd != fdsrv)
	return -1;

    /* receive the file descriptor */
    recv_fd(&received_fd, fdsrv);

    /* send it to erlang as a string */
    sprintf(buf, "%d", received_fd);
    len = strlen(buf);
    driver_output(port, buf, len);
    
    return 0;
}

/*
 * Called when the Erlang port is closed.
 */
static int stop()
{
    /* make sure we stop Erlang from selecting on fdsrv */
    driver_select(instance, fdsrv, DO_READ, 0);

    /* cleanup */
    close(fdsrv);
    instance = -1;
    
    return 0;
}


/*
 * The code that actually receives a file descriptor over a AF_UNIX
 * stream socket. More or less directly from Stevens.
 */

#ifdef CMSG_FIRSTHDR
#define HAVE_MSGHDR_MSG_CONTROL
#endif

#ifdef HAVE_MSGHDR_MSG_CONTROL
/* We need the newer CMSG_LEN() and CMSG_SPACE() macros, but few
   implementations support them today.  These two macros really need
    an ALIGN() macro, but each implementation does this differently. */
#ifndef CMSG_LEN
#define CMSG_LEN(size)          (sizeof(struct cmsghdr) + (size))
#endif
#ifndef CMSG_SPACE
#define CMSG_SPACE(size)        (sizeof(struct cmsghdr) + (size))
#endif

/* These two macros are really needed as well */
#ifndef CMSG_FIRSTHDR
#define CMSG_FIRSTHDR(mhdr) \
  ((size_t) (mhdr)->msg_controllen >= sizeof (struct cmsghdr)		      \
   ? (struct cmsghdr *) (mhdr)->msg_control : (struct cmsghdr *) NULL)
#endif
#ifndef CMSG_DATA
#define CMSG_DATA(cmsg) ((unsigned char *) ((struct cmsghdr *) (cmsg) + 1))
#endif

#endif

static void recv_fd(fd, sock_fd)
    int *fd;
    int sock_fd;
{
    struct iovec iov[1];
    struct msghdr msg;
    int res;
    char c;
#ifdef HAVE_MSGHDR_MSG_CONTROL
    union {
	struct cmsghdr cm;
	char control[CMSG_SPACE(sizeof(int))];
    } control_un;
    struct cmsghdr *cmptr;

    msg.msg_control = control_un.control;
    msg.msg_controllen = sizeof(control_un.control);
#else
    int newfd;

    msg.msg_accrights = (caddr_t) &newfd;
    msg.msg_accrightslen = sizeof(int);
#endif
    msg.msg_name = NULL;
    msg.msg_namelen = 0;

    iov[0].iov_base = &c;
    iov[0].iov_len = 1;
    msg.msg_iov = iov;
    msg.msg_iovlen = 1;

    if ((res = recvmsg(sock_fd, &msg, 0)) < 0) {
	perror("recvmsg");
	goto error;
    }
    /* printf("recvmsg() -> %d (flags = %d)\n", res, msg.msg_flags); */
    if (res != 1) {
	fprintf(stderr, "recvmsg() unexpected return value (%d)\n", res);
	goto error;
    }
#ifdef HAVE_MSGHDR_MSG_CONTROL
    if ( (cmptr = CMSG_FIRSTHDR(&msg)) != NULL
#ifdef BROKEN_CMSG_FIELDS
	) {
#else
	  && cmptr->cmsg_len == CMSG_LEN(sizeof(int))) {
	if (cmptr->cmsg_level != SOL_SOCKET || cmptr->cmsg_type != SCM_RIGHTS){
	    fprintf(stderr, "level or controle type error!\n");
	    goto error;
	}
#endif
	*fd = *((int *) CMSG_DATA(cmptr));
    } else {
	fprintf(stderr, "error in received fd! (cmptr 0x%lx, cmsg_len %d)\n",
		(unsigned long)cmptr, cmptr ? cmptr->cmsg_len : -1);
	goto error;
    }
#else
    if (msg.msg_accrightslen == sizeof(int)) {
	*fd = newfd;
    } else {
	fprintf(stderr, "error in received fd! (%d != %d",
		msg.msg_accrightslen, sizeof(int));
	goto error;
    }
#endif
    return;
error:
    *fd = -1;
    return;
}


int null_func() { return 0; }

#ifdef TEST
/*
 * Define TEST to get a standalone program that can be used to test
 * the functionality.
 *
 */

/* needed by driver code above */

/* needed by driver code above */
int driver_select(i1, i2, i3, i4) { return 0; }

/*
 * Called by the driver with the received file descriptor as a string.
 * Put the descriptor in the static variable rfd.
 */
static int rfd;

int driver_output(port, buf, len)
    char *buf;
    int len;
{
    rfd = atoi(buf);
    return 0;
}

static void echo_test(fd, spec)
    int fd;
    char *spec;
{
    char tmpbuf[512];
    unsigned char c;
    int sfd, r;

    fprintf(stderr, "\nSpec is %s\n", spec);
    
    c = strlen(spec);
    write(fd, &c, 1);
    write(fd, spec, strlen(spec));

    fd_is_ready(instance, fdsrv);

    fprintf(stderr, "Received fd is %d\n", rfd);

    fprintf(stderr, "listen/"); fflush(stderr);
    if (listen(rfd, 2) < 0) {
	perror("listen");
	close(rfd);
	return;
    }
    
    fprintf(stderr, "accept/"); fflush(stderr);
    sfd = accept(rfd, (struct sockaddr *)NULL, (int *)0);
    if (sfd < 0) {
	perror("accept");
	close(rfd);
	return;
    }
    close(rfd);

    fprintf(stderr, "read/"); fflush(stderr);
    r = read(sfd, tmpbuf, 512);
    if (r < 0) {
	perror("read");
	close(sfd);
	return;
    }
    fprintf(stderr, "write\n"); fflush(stderr);
    write(sfd, tmpbuf, r);
    close(sfd);
    
    return;
}


int main(argc, argv)
    int argc;
    char *argv[];
{
    int i, portfd[2];
    pid_t pid;
    char path[64];

    /* make a filename for the named pipe */
    sprintf(path, "/tmp/fdsrv%d", (int)getpid());

    /*
     * Run fdsrv as Erlang does, i.e. at the other end of a pipe. (Well two
     * pipes, but we aren't interested in data from fdsrv so just let it
     * spill out on stdout.)
     */
    if (pipe(portfd) < 0) {
	perror("pipe");
	exit(1);
    }

    if ((pid = fork()) < 0) {
	perror("fork");
	exit(1);
    }
    if (pid == 0) {		/* child */
	close(portfd[1]);
	dup2(portfd[0], 0);
	close(portfd[0]);
	execl("./fdsrv", "fdsrv", path, NULL);
	perror("execl");
	exit(1);
    }
    /* parent */
    close(portfd[0]);
    sleep(1);
    fprintf(stderr, "\n");

#if 0
    {
	int i;
	for(i=0; i<59; i++)
	    if (open("/dev/null", 0, 0)<0) perror("open");
    }
#endif

    start(1l, path);

    /* Accept specs on command line */
    if (argc > 1) {
	for (i=1; i<argc; i++)
	    echo_test(portfd[1], argv[i]);
    } else {
	echo_test(portfd[1], ":9999");
	echo_test(portfd[1], ":9998");
    }

    stop();
    close(portfd[1]);
    exit(0);
}

#endif
