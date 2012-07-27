
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
### $Source: /cvsroot/eddie/eddie/inet_server/src/Makefile,v $ 
### $Revision: 1.1 $ 
### $Date: 2000/10/27 22:20:25 $ 
### $State: Exp $ 
### --------------------------------------------------------------------

# Variable to be provided
EDDIE_ROOT = ../..

include $(EDDIE_ROOT)/eddie.mk

include $(EDDIE_ROOT)/vsn.mk
VSN=$(INET_SERVER_VSN)

APP_FILE= inet_server.app

APP_SRC= $(APP_FILE).src
APP_TARGET= ../ebin/$(APP_FILE)

MODULES=erlet_utils \
	inet_app \
	inet_server \
	is_config \
	is_core \
	is_db \
	is_service_db \
	is_sup \
	srv_parse \
	srv_table \
	http_fields \
	url_parse \
	queue_srv \
	queue_timer \
	http \
	http_admit \
	http_db \
	http_parse \
	erlet_status \
	erlet_admit_ctrl

HRL_FILES=inet_server.hrl \
	erlet.hrl \
	srv_parse.hrl

TARGET_FILES= $(MODULES:%=../ebin/%.$(EMULATOR)) $(APP_TARGET) 

all: $(TARGET_FILES)

$(APP_TARGET): $(APP_SRC) $(EDDIE_ROOT)/vsn.mk
	sed -e 's;%VSN%;$(VSN);' $(APP_SRC) > $(APP_TARGET)

clean:
	rm -f $(TARGET_FILES)


