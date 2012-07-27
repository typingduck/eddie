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
 * fdsrv.c
 *
 * Author: Sebastian Strollo <seb@erix.ericsson.se>
 *
 * Program that passes open file descriptors between processes using
 * AF_UNIX stream sockets, as described in Stevens UNIX Network programming.
 *
 * This is a server program which can be run as setuid root to give it
 * access to privilidged ports. The program is meant to be run from erlang
 * with 1 byte length packets.
 */
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <strings.h>
#include <unistd.h>
#include <netdb.h>
#ifdef __FreeBSD__
#include <sys/param.h>        /* include ALIGN macro */
#endif
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/uio.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#ifdef __FreeBSD__
#undef BSD            /* Remove BSD symbol conflict */
#endif
#include "config.h"

/* sigh */
#ifndef HAVE_SYS_ERRLIST
extern char *sys_errlist[];
#endif

/* should be, but isn't on all systems, defined in <netinet/in.h> */
#ifndef INADDR_NONE
#define INADDR_NONE 0xffffffff
#endif

static uid_t saved_euid;
static char *progname;

/*
 * Create a named pipe with name "path". Accept connection on it and then
 * close it and unlink "path".
 */
static int establish_communication(path)
    char *path;
{
    char buf[4];
    int c, s, len;
    struct sockaddr_un addr;

    s = socket(AF_UNIX, SOCK_STREAM, 0);
    bzero((char *)&addr, sizeof(addr));
    addr.sun_family = AF_UNIX;

    /* make sure we don't overrun */
    if (strlen(path) > (sizeof(addr.sun_path) + 1)) {
	fprintf(stderr, "%s: path too long: %s\n", progname, path);
	exit(1);
    }
    strcpy(addr.sun_path, path);
    len = strlen(addr.sun_path) + sizeof(addr.sun_family) + 1;

    if (bind(s, (struct sockaddr*)&addr, len) < 0) {
	perror("bind (named pipe)");
	exit(1);
    }

    /* Limit access to the pipe */
    if (chmod(path, S_IRUSR|S_IWUSR) < 0)
	perror("chmod");

    /* An OS who shall remain unnamed creates files with owner set to euid
     * instead of uid. Undo the damage.
     */
    if (chown(path, getuid(), getgid()) < 0)
	perror("chown");

    listen(s, 1);

    /* We need to synchronize the creation of the named pipe - so that the
     * client does not try to connect to it until it is created.
     * This is used as synchronization.
     */
    buf[0] = 2; buf[1] = 'o'; buf[2] = 'k';
    write(1, buf, 3);

    len = 0;
    if ((c = accept(s, (struct sockaddr *)NULL, &len)) < 0) {
	perror("accept (named pipe)");
	exit(1);
    }

    /* Communication established, no need to listen to the pipe anymore,
     * or to have it lying around in the filesystem.
     */
    close(s);
    unlink(path);
    
    return c;
}

/*
 * Function to send a file descriptor, fd, over a stream socket, sock_fd.
 * This function is more or less directly from Stevens.
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

static void send_fd(fd, sock_fd)
    int *fd;
    int sock_fd;
{
    struct msghdr msg;
    struct iovec iov[1];
#ifdef HAVE_MSGHDR_MSG_CONTROL
    union {
	struct cmsghdr cm;
	char control[CMSG_SPACE(sizeof(int))];
    } control_un;
    struct cmsghdr *cmptr;

    msg.msg_control = control_un.control;
    msg.msg_controllen = sizeof(control_un.control);

    cmptr = CMSG_FIRSTHDR(&msg);
    cmptr->cmsg_len = CMSG_LEN(sizeof(int));
    cmptr->cmsg_level = SOL_SOCKET;
    cmptr->cmsg_type = SCM_RIGHTS;
    *((int *) CMSG_DATA(cmptr)) = *fd;
#else
    msg.msg_accrights = (caddr_t) fd;
    msg.msg_accrightslen = sizeof(*fd);
#endif

    iov[0].iov_base = "";	/* send one byte */
    iov[0].iov_len = 1;
    msg.msg_iov = iov;
    msg.msg_iovlen = 1;
    msg.msg_name = NULL;
    msg.msg_namelen = 0;
    
    if (sendmsg(sock_fd, &msg, 0) < 0) {
	perror("sendmsg");
	exit(1);
    }
}

/*
 * Parse an address specification and fill in a sockaddr_in accordingly.
 * The address should be on the form:
 *   [a.d.re.ss|hostname]:{portnumber|servicename}
 */
static int parse_addr(addr, str)
    struct sockaddr_in *addr;
    char *str;
{
    int port = 0;
    char *cp;
    struct hostent *hp;
    struct servent *se;

    if ((cp = strrchr(str, (int)':')) != NULL)
        *cp++ = '\0';
    if (cp) {
        if (!isdigit((int)cp[0])) {
            if ((se = getservbyname(cp, "tcp")) != NULL) {
                port = ntohs(se->s_port);
	    } else {
		/* fprintf(stderr, "unknown port %s\n", cp); */
		return -1;
	    }
        } else {
            port = atoi(cp);
        }
    }
    if (port < 0 || port > 0xffff) {
	/* fprintf(stderr, "bad port number %d\n", port); */
        return -1;
    }
    
    bzero(addr, sizeof(*addr));
    addr->sin_family = AF_INET;
    addr->sin_port = htons(port);
    if (*str == '\000') {
	addr->sin_addr.s_addr = INADDR_ANY;
    } else {
	if ((addr->sin_addr.s_addr = inet_addr(str)) == INADDR_NONE) {
	    if ((hp = gethostbyname(str)) == NULL) {
		/*fprintf(stderr, "\"%s\" unknown host or address!\n", str);*/
		return -1;
	    } else {
		bcopy(hp->h_addr_list[0], &addr->sin_addr.s_addr,hp->h_length);
	    }
	}
    }
    return 0;
}

/*
 * Send an error message to Erlang. (If msg is NULL, send errno error.)
 */
static void report_error(msg)
    char *msg;
{
    unsigned char c;
    
    if (!msg)
	msg = sys_errlist[errno];
    if (strlen(msg) > 255)
	c = 255;
    else
	c = (unsigned char)strlen(msg);
    write(1, &c, 1);
    write(1, msg, (int)c);
    return;
}

static void loop(fd)
    int fd;
{
    int i, r, s;
    char buf[256];
    struct sockaddr_in addr;

    for(;;) {
	/*
	 * Read address specs on stdin.
	 */
	if ((r = read(0, buf, 1)) == 0)
	    return;
	if (r != 1) {
	    perror("read");
	    exit(1);
	}
	i = (int)((unsigned int)buf[0]);
	if ((r = read(0, buf, i)) == 0)
	    return;
	if (r != i) {
	    perror("read");
	    exit(1);
	}
	buf[i] = 0;		/* null terminate string */

	/* buf[0] contains protocol opcode */
	if (parse_addr(&addr, &buf[1]) < 0) {
	    report_error("Couldn't parse address");
	    continue;
	}

	/*
	 * Create the socket to be passed.
	 */
	if (buf[0] == 0) {
	    /* tcp */
	    if ((s = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
	        /* perror("socket"); */
	        report_error((char *)NULL);
		continue;
	    }
	    i = 1; setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (char *)&i,
			      sizeof(i));
	}
	else {
	    /* udp */
	    if ((s = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
	        /* perror("socket"); */
	        report_error((char *)NULL);
		continue;
	    }
	}

	/*
	 * Bind it according to spec.
	 */
	/*seteuid(saved_euid);*/  /* flip euid, XXX doesn't work on Solaris */
	r = bind(s, (struct sockaddr *)&addr, sizeof(addr));
	/* seteuid(getuid()); */

	if (r < 0) {
	    /* perror("bind"); */
	    report_error((char *)NULL);
	    continue;
	}

	/*
	 * Send it away, and then close our copy.
	 */
	send_fd(&s, fd);
	close(s);
    }
}

int main(argc, argv)
    int argc;
    char *argv[];
{
    int fd;
    
    if (argc != 2)
	exit(1);

    saved_euid = geteuid();
    /* seteuid(getuid()); */

    if ((progname = strrchr(argv[0], (int)'/')) == NULL)
	progname = argv[0];
    else
	progname++;

    fd = establish_communication(argv[1]);

    loop(fd);

    exit(0);
}
