### --------------------------------------------------------------------
###
### The contents of this file are subject to the Erlang Public License,
### Version 1.0, (the "License"); you may not use this file except in
### compliance with the License. You may obtain a copy of the License at
### http://www.eddieware.org/EPL
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
### Contributor(s): 
###     Aug99 Geoff Wong
###
### $Id: Makefile,v 1.1 2000/10/27 22:20:27 dredd Exp $ 
### $Source: /cvsroot/eddie/eddie/servant/src/Makefile,v $ 
### $Revision: 1.1 $ 
### $Date: 2000/10/27 22:20:27 $ 
### $State: Exp $ 
### --------------------------------------------------------------------

EDDIE_ROOT = ../..

include $(EDDIE_ROOT)/eddie.mk

include $(EDDIE_ROOT)/vsn.mk

APP_FILES=servant 

MODULES=  \
    servant_app \
	servant_config \
	servant_inet_server_sup \
	servant_monitor_server \
	servant_monitor_sup \
	servant_server \
	servant_sup \
	servant_util 

EBIN_FILES=$(MODULES:%=../ebin/%.$(EMULATOR)) $(APP_FILES:%=../ebin/%.app)

ifeq ($(findstring cygwin,$(TARGET_OS)), cygwin)
PRIV_FILES=
else
PRIV_FILES=../priv/ifadd ../priv/ifdel ../priv/ifget ../priv/routeadd ../priv/garp
endif

all: $(EBIN_FILES) $(PRIV_FILES) 

clean:
	rm -f $(EBIN_FILES) $(PRIV_FILES)

# Erlang

../ebin/servant_config.$(EMULATOR): ../../misc/include/logger.hrl ../../servant/include/db.hrl 

../ebin/servant_monitor_server.$(EMULATOR): ../../servant/include/db.hrl ../../misc/include/logger.hrl

../ebin/servant_server.$(EMULATOR): ../../servant/include/db.hrl ../../misc/include/ip.hrl ../../misc/include/logger.hrl

../ebin/servant_util.$(EMULATOR): ../../servant/include/db.hrl ../../misc/include/ip.hrl

# C

../priv/garp: garp.c rights.c garp.h
	$(CC) $(CFLAGS) garp.c rights.c -o $@ -lm

../priv/ifget: ifget.c err.c rights.c ifget.h
	$(CC) $(CFLAGS) ifget.c err.c rights.c -o $@ $(LIBS)

../priv/%: %.c rights.c
	$(CC) $(CFLAGS) $< rights.c -o $@

# Application file stuff

../ebin/%.app: %.app.src
	sed -e 's;%VSN%;$(SERVANT_VSN);' $? > $@


