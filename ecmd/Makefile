
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
### Author: tobbe@eddieware.org
### Contributor(s): 
###
### $Id: Makefile,v 1.1 2000/10/27 22:20:24 dredd Exp $ 
### $Source: /cvsroot/eddie/eddie/ecmd/Makefile,v $ 
### $Revision: 1.1 $ 
### $Date: 2000/10/27 22:20:24 $ 
### $State: Exp $ 
### --------------------------------------------------------------------

# Variable to be provided
EDDIE_ROOT=..

include $(EDDIE_ROOT)/eddie.mk
include $(EDDIE_ROOT)/vsn.mk

ifeq ($(TARGET_OS),bsd)
   LIBS      = -lerl_interface -lei
else
   LIBS      = -L/usr/local/lib/erlang/lib/erl_interface-3.2.3/lib -lerl_interface -lei -lresolv 
endif

ifeq ($(findstring cygwin,$(TARGET_OS)),cygwin)
WIN_TARGET = ../bin/ecmd.exe

all: $(WIN_TARGET)

$(WIN_TARGET): ecmd.c
	cl /D__WIN32__ /I$(ERLDIR)/lib/erl_interface-3.2.3/include /Fe../bin/ecmd.exe ecmd.c wsock32.lib  $(ERLDIR)/usr/lib/erl_interface.lib $(ERLDIR)/usr/lib/ei.lib /MT

else
all: ecmd.o
	$(LINK.c) -L$(ERLDIR)/usr/lib -o ../bin/ecmd ecmd.o $(LIBS)
endif

clean:
	rm -f ../bin/ecmd ecmd.o 


