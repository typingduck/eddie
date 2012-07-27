/*
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.0, (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.eddieware.org/EPL
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * The Initial Developer of the Original Code is Ericsson Telecom
 * AB. Portions created by Ericsson are Copyright (C), 1998, Ericsson
 * Telecom AB. All Rights Reserved.
 * 
 * Contributor(s): ______________________________________.
 */

/*
 * syslog_drv.c
 *
 * Author: Geoff Wong <geoff@eddieware.org>
 *
 * This is an erlang driver which receives syslog commands (essentially)
 * and invokes the C syslog call to log stuff.
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
#include <syslog.h>
#include "config.h"
#include "erl_driver.h"		/* Erlang driver definitions */

/* forward declarations */
static ErlDrvData log_start(ErlDrvPort port, char * buf);
static void log_stop();
static void log_ready(int port, int readyfd);
static void log_output();
static void recv_log(int sock_fd, int * priority, char * buf);

/* driver global variables */
static int instance = -1;
static int log_port;

static int null_func() { return 0; }
static void null_void() { }

/* driver structure entry */
struct erl_drv_entry syslog_entry = 
{
    null_func,
    log_start,
    log_stop,
    log_output,
    null_void,
    null_void,
    "syslog_drv"
};

ErlDrvEntry *driver_init(void * handle)
{
    syslog_entry.finish = null_func;
    syslog_entry.handle = handle;
    return &syslog_entry;
}

/*
 * Called when erlang side does "open_port()".
 */
static ErlDrvData log_start(ErlDrvPort port, char * buf)
{
    log_port = port;

#if 0
    int len;
    char *path;
    struct sockaddr_un addr;


    /* only allow one copy at the time */
    if (instance != -1)		return -1;
    if (strlen(buf) > (sizeof(addr.sun_path) + 1)) return -1;
    instance = port;

    /* Figure out the path to the named socket */
    if ((path = strrchr(buf, (int)' ')) == NULL) path = buf;
    else path++;

    fdsrv = socket(AF_UNIX, SOCK_STREAM, 0);
    bzero((char *)&addr, sizeof(addr));
    addr.sun_family = AF_UNIX;
    strcpy(addr.sun_path, path);
    len = strlen(path) + sizeof(addr.sun_family) + 1;

    if (connect(fdsrv, (struct sockaddr *)&addr, len) < 0) 
    {
        perror("connect");
        instance = -1;
        return -1;
    }

    
    /* Have Erlang select on fdsrv */
    driver_select(port, fdsrv, DO_READ, 1);
#endif
    /* Open syslog */
    openlog("Eddie", 0, LOG_DAEMON);

    return port;
}

/*
 * log_output:
 * Called when erlang wants to send output to the port
 */

static void log_output(long port, char * buf, int len)
{
    int priority, amt;

    if (!buf) return;

    switch (buf[0])
    {
        case 'D':   /* debug */
            priority = LOG_DAEMON | LOG_DEBUG;
            break;
        case 'E':   /* error */
            priority = LOG_DAEMON | LOG_ERR;
            break;
        case 'F':   /* fatal */
            priority = LOG_DAEMON | LOG_CRIT;
            break;
        case 'I':   /* info */
            priority = LOG_DAEMON | LOG_INFO;
            break;
        default:
            priority = LOG_DAEMON | LOG_NOTICE;
            break;
    }

    syslog(priority, &(buf[1]));

    return;
}

/*
 * Called when select triggers. Which is when there is an incomming fd.
 */

#define MAX_LOG 1024
static void log_ready(int port, int readyfd)
{
    int received_fd;
    int len;
    char buf[MAX_LOG];
	
#if 0
    if (port != instance) return -1;
    if (readyfd != fdsrv) return -1;

    /* receive the file descriptor */
    recv_log(readyfd, &priority, buf);

    /* syslog it ..*/
    syslog(priority, buf);
#endif
    
    return;
}

/*
 * Called when the Erlang port is closed.
 */
static void log_stop()
{
#if 0
    /* make sure we stop Erlang from selecting on fdsrv */
    driver_select(instance, fdsrv, DO_READ, 0);
    instance = -1;
#endif

    closelog();

    /* cleanup */
    close(log_port);
    
    return;
}


static void recv_log(int sock_fd, int * priority, char * buf)
{
#if 0
    struct iovec iov[1];
    struct msghdr msg;
    int res;
    char c;
    int newfd;

    msg.msg_accrights = (caddr_t) &newfd;
    msg.msg_accrightslen = sizeof(int);
    msg.msg_name = NULL;
    msg.msg_namelen = 0;

    iov[0].iov_base = &c;
    iov[0].iov_len = 1;
    msg.msg_iov = iov;
    msg.msg_iovlen = 1;

    if ((res = recvmsg(sock_fd, &msg, 0)) < 0) 
    {
        perror("recvmsg");
        goto error;
    }

    /* printf("recvmsg() -> %d (flags = %d)\n", res, msg.msg_flags); */
    if (res != 1) 
    {
        fprintf(stderr, "recvmsg() unexpected return value (%d)\n", res);
        goto error;
    }
    
    if (msg.msg_accrightslen == sizeof(int)) 
    {
	    *fd = newfd;
    } 
    else 
    {
        fprintf(stderr, "error in received fd! (%d != %d",
            msg.msg_accrightslen, sizeof(int));
        goto error;
    }
    return;
error:
    *fd = -1;
    return;
#endif
}


