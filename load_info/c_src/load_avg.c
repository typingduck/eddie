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
 * load_avg.c
 *
 * Author: Magnus Fröberg <magnus@erix.ericsson.se>
 *
 * Fetch the load average on request from the Erlang side (FreeBSD only).
 *
 * Keep two decimals if value is lesser than 1000.
 */
   
#include <math.h>

#include <sys/types.h>
#include <unistd.h>

#define MAX_LEN 5
unsigned char msg[MAX_LEN];

#define GET_LOAD 0

#define KEEP_DEC 0
#define NO_DEC   1

#define NULLFDS ((fd_set *) NULL)
#define NULLTV ((struct timeval *) NULL)

/* s[0] is already in use */
#define PUT_INT32(s,x) do { s[1] = ((x) >> 24) & 0xff; \
			    s[2] = ((x) >> 16) & 0xff; \
			    s[3] = ((x) >> 8) & 0xff; \
			    s[4] = ((x) & 0xff); } while(0)

static int erl_write(fd, opcode, message)
     int fd;
     int opcode;
     long message;
{
  msg[0] = opcode;
  PUT_INT32(msg, message);
  write(fd, msg, 5);
  return(0);
}

int main(argc, argv)
    int argc;
    char *argv[];
{
  fd_set read_fds;
  int erlin_fd = 0;
  int erlout_fd = 1;
  int max_fd, i;
  double la[1];

  while(1) {
    max_fd = erlin_fd;
    FD_ZERO(&read_fds);
    FD_SET(erlin_fd, &read_fds);
    if ((i = select(max_fd + 1, &read_fds, NULLFDS, NULLFDS, NULLTV)) < 0) {
      return(-1);
    }
    else {
      if (FD_ISSET(erlin_fd, &read_fds)) {
	if (read(erlin_fd, &msg, 1) < 1) {
	  /* Erlang has closed */
	  return(0);
	}
	else if (msg[0] == GET_LOAD) {
	  if (getloadavg(&la, 1) < 1) {
	    erl_write(erlout_fd, 0);
	  }
	  else if (la[0] < 1000) {
	    /* Keep two decimals */
	    erl_write(erlout_fd, KEEP_DEC, (long) floor(100*la[0]));
	  }
	  else {
	    erl_write(erlout_fd, NO_DEC, (long) floor(la[0]));
	  }
	}
	else
	  /* junk sent from Erlang */
	  return(-1);
      }
    }
  }
}



