
### --------------------------------------------------------------------
### Author: Sebastian Strollo <seb@erix.ericsson.se>
### Author: Magnus Fröberg <magnus@erix.ericsson.se>
###
### The contents of this file are subject to the Erlang Public License
### License, Version 1.0, (the "License"); you may not use this file
### except in compliance with the License. You may obtain a copy of the
### License at http://www.eddieware.org/EPL
###
### Software distributed under the License is distributed on an "AS IS"
### basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
### the License for the specific language governing rights and limitations
### under the License.
###
### The Original Code is Eddie-0.83b1.
###
### The Initial Developer of the Original Code is Ericsson Telecom
### AB. Portions created by Ericsson are Copyright (C), 1998,1999 Ericsson
### Telecom AB. All Rights Reserved.
###
### Contributor(s): ______________________________________.
###
### $Id: Makefile,v 1.1 2000/10/27 22:20:26 dredd Exp $ 
### $Source: /cvsroot/eddie/eddie/misc/c_src/Makefile,v $ 
### $Revision: 1.1 $ 
### $Date: 2000/10/27 22:20:26 $ 
### $State: Exp $ 
### --------------------------------------------------------------------

#
# fdsrv is a program that passes open file descriptors between processes using
# AF_UNIX stream sockets, as described in Stevens UNIX Network programming.
#
# Run make {freebsd|linux} ERLDIR=/your/erl/dir to compile on your faorite OS.
#

# Variable to be provided
EDDIE_ROOT = ../..

include $(EDDIE_ROOT)/eddie.mk

ifeq ($(findstring cygwin,$(TARGET_OS)),cygwin)
PRIV_FILES=../priv/sig_handler
else
PRIV_FILES=../priv/sig_handler ../priv/fdsrv ../priv/fdsrv_drv.so ../priv/syslog_drv.so
endif

#LIBS      = -lnsl -lsocket 
LD_SHARED = ld -G
DEFS      =

all: $(PRIV_FILES) 

bsd:
	$(MAKE) LIBS= LD_SHARED="ld -Bshareable"

linux:
	$(MAKE) LIBS=$(TARGET)

## Tue Mar 30 13:30:11 EST 1999 tobbe@eddieware.org
## NB: I'll remove this one for now. Should probably do a
##     better test for this in the $(EDDIE_ROOT)/configure
#	$(MAKE) DEFS="-DBROKEN_CMSG_FIELDS" LIBS=

## S.u.S.E. linux
##
## Tue Mar 30 13:30:11 EST 1999 tobbe@eddieware.org
## NB: I'll remove this one for now. Should probably do a
##     better test for this in the $(EDDIE_ROOT)/configure
##
#suse:
#	$(MAKE) DEFS="-DHAVE_SYS_ERRLIST -DBROKEN_CMSG_FIELDS -DHAVE_MSGHDR_MSG_CONTROL" LIBS= 

solaris:
	$(MAKE)

## Windows NT under Cygnus tools
## Does not do anything yet.
cygwin:
	$(MAKE)

install: ../priv/fdsrv
	su root -c \
	  "(chown root ../priv/fdsrv; chmod 4755 ../priv/fdsrv)"

remove:
	su root -c "rm -f ../priv/fdsrv"

../priv/fdsrv: fdsrv.c
	$(CC) -o $@ $(CFLAGS) $(DEFS) fdsrv.c $(LIBS)

../priv/%.so: %.o
	$(LD_SHARED) -o $@ $<

%.o: %.c
	$(CC) -o $@ -c -fpic $(CFLAGS) $(DEFS) -DDYNAMIC_DRIVER $<

../priv/sig_handler: sig_handler.c
	$(CC) sig_handler.c -o $@

## Test purposes
../priv/fdsrv_drv: fdsrv_drv.c
	$(CC) -o $@ $(CFLAGS) $(DEFS) -DTEST fdsrv_drv.c $(LIBS)

clean:
	rm -f $(PRIV_FILES) *.o *.so *~


