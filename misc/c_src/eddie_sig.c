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
  This is a signal handling process which can be used in order to send
  signals to an erlang system.
  This process is running as long as the fd to erlang is open. 
  
  Define own ERL_SIGHUP etc due to different values on diff. OS'es,
  e.g. on Solaris SIGUSR1 = 16 but on Linux SIGUSR1 = 10.
*/
   
#include <sys/types.h>
#include <sys/time.h>
#include <unistd.h>
#include <signal.h>

int got_sig = 0;

#define MAX_LEN 4
unsigned char msg[MAX_LEN];

void sig_handler();

#define NULLFDS ((fd_set *) NULL)
#define NULLTV ((struct timeval *) NULL)

#define PUT_INT16(s,x) do { s[0] = ((x) >> 8) & 0xff; \
			    s[1] = ((x) & 0xff); } while(0)

#define PPUT_INT16(s,x) do { s[2] = ((x) >> 8) & 0xff; \
			     s[3] = ((x) & 0xff); } while(0)

#define ERL_SIGHUP  1
#define ERL_SIGINT  2
#define ERL_SIGABRT 3
#define ERL_SIGUSR1 4
#define ERL_SIGUSR2 5

int main(int argc, char **argv){
  fd_set read_fds;
  int erlin_fd = 0;
  int erlout_fd = 1;
  int max_fd, i;

  /* Send our pid to Erlang first */
  PUT_INT16(msg, getpid());
  PPUT_INT16(msg, getppid());
  write(erlout_fd, msg, 4);

  while(1) {
    signal(SIGHUP, sig_handler);
    signal(SIGINT, sig_handler);
    signal(SIGABRT, sig_handler);
    signal(SIGUSR1, sig_handler);
    signal(SIGUSR2, sig_handler);

    max_fd = erlin_fd;
    FD_ZERO(&read_fds);
    FD_SET(erlin_fd, &read_fds);
    if ((i = select(max_fd + 1, &read_fds, NULLFDS, NULLFDS, NULLTV)) < 0) {
      switch (got_sig) {
      case SIGHUP:
	erl_write(erlout_fd, ERL_SIGHUP);
	break;
      case SIGINT:
	erl_write(erlout_fd, ERL_SIGINT);
	break;
      case SIGABRT:
	erl_write(erlout_fd, ERL_SIGABRT);
	break;
      case SIGUSR1:
	erl_write(erlout_fd, ERL_SIGUSR1);
	break;
      case SIGUSR2:
	erl_write(erlout_fd, ERL_SIGUSR2);
	break;
      default:
	break;
      }
    }
    else {
      if(FD_ISSET(erlin_fd, &read_fds)) {
	/* If something on erlin_fd we treat it as closed ! */
	return(0);
      }
    }
  }
}

int erl_write(int fd, int message) {
  msg[0] = message;
  write(fd, msg, 1);
}


void sig_handler(int sig) {
  got_sig = sig;
}
