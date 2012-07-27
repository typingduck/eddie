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
### Contributor(s): ______________________________________.
###
### $Id: Makefile,v 1.1 2000/10/27 22:20:25 dredd Exp $ 
### $Source: /cvsroot/eddie/eddie/load_info/src/Makefile,v $ 
### $Revision: 1.1 $ 
### $Date: 2000/10/27 22:20:25 $ 
### $State: Exp $ 
### --------------------------------------------------------------------

# Variable to be provided
EDDIE_ROOT = ../..

include $(EDDIE_ROOT)/eddie.mk

include $(EDDIE_ROOT)/vsn.mk
VSN=$(LOAD_INFO_VSN)

MODULES= load_server load_sup load_app lonelb

ERL_FILES= $(MODULES:%=%.erl)

APP_FILE= load_info.app

APP_SRC= $(APP_FILE).src
APP_TARGET= ../ebin/$(APP_FILE)

TARGET_FILES= $(MODULES:%=../ebin/%.$(EMULATOR)) $(APP_TARGET)

all: $(TARGET_FILES)

clean:
	rm -f $(TARGET_FILES)
	rm -f core

$(APP_TARGET): $(APP_SRC) $(EDDIE_ROOT)/vsn.mk
	sed -e 's;%VSN%;$(VSN);' $(APP_SRC) > $(APP_TARGET)

../ebin/load_server.($EMULATOR): load_server.hrl


