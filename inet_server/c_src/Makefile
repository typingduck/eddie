### --------------------------------------------------------------------
### The contents of this file are subject to the Erlang Public License,
### Version 1.0, (the "License"); you may not use this file except in
### compliance with the License. You may obtain a copy of the License at
### http://www.eddieware.org/EPL
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
### Author(s): rob@eddieware.org
###
### $Id: Makefile,v 1.1 2000/10/27 22:20:24 dredd Exp $ 
### $Source: /cvsroot/eddie/eddie/inet_server/c_src/Makefile,v $ 
### $Revision: 1.1 $ 
### $Date: 2000/10/27 22:20:24 $ 
### $State: Exp $ 
### --------------------------------------------------------------------

# Variable to be provided
EDDIE_ROOT = ../..

CC = gcc
CFLAGS = -g -Wall

include $(EDDIE_ROOT)/eddie.mk
include $(EDDIE_ROOT)/vsn.mk

SANITY_SRC = sanity_check.c
BASE_SRC = server.c parent.c backend.c control.c child.c param.c
PLAIN_SRC = main.c plain_sup.c

SANITY_OBJ = $(SANITY_SRC:.c=.o)
BASE_OBJ = $(BASE_SRC:.c=.o)
PLAIN_OBJ = $(PLAIN_SRC:.c=.o)

DEP = $(SANITY_SRC:%.c=.%.d) $(BASE_SRC:%.c=.%.d) $(PLAIN_SRC:%.c=.%.d)

.PHONY: all clean

all: relay 

relay: sanity_check $(BASE_OBJ) $(PLAIN_OBJ)
	./sanity_check
	$(CC) $(CFLAGS) -o $@ $(LDFLAGS) $(BASE_OBJ) $(PLAIN_OBJ) $(LIBS) 
	-cp relay$(EXE) ../priv

sanity_check: $(SANITY_OBJ)
	$(CC) -o $@ $(LDFLAGS) $(SANITY_OBJ) $(LIBS) 

tags: $(BASE_SRC) $(PLAIN_SRC) 
	ctags $^

clean:
	rm -f $(BASE_OBJ) $(PLAIN_OBJ) $(DEP) tags relay sanity_check

%.o: %.c
	$(CC) -c $(CFLAGS) $<

.%.d: %.c
	(printf "%s" '$@ '; $(CC) -MM $(CFLAGS) $< ) > $@

ifneq ($(MAKECMDGOALS),clean)
include $(DEP)
endif
