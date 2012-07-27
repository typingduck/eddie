
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
### $Id: Makefile,v 1.1 2000/10/27 22:20:26 dredd Exp $ 
### $Source: /cvsroot/eddie/eddie/master/src/Makefile,v $ 
### $Revision: 1.1 $ 
### $Date: 2000/10/27 22:20:26 $ 
### $State: Exp $ 
### --------------------------------------------------------------------

# Variable to be provided
EDDIE_ROOT = ../..

include $(EDDIE_ROOT)/eddie.mk

include $(EDDIE_ROOT)/vsn.mk

APP_FILE=master.app

MODULES=master_app master_heuristics master_server master_sup

EBIN_FILES=$(MODULES:%=../ebin/%.$(EMULATOR)) ../ebin/$(APP_FILE)

PRIV_FILES=

all: $(EBIN_FILES) $(PRIV_FILES) 

clean:
	rm -f $(EBIN_FILES) $(PRIV_FILES)

# Erlang

../ebin/master_heuristics.$(EMULATOR): ../../servant/include/db.hrl

../ebin/master_server.$(EMULATOR): ../../servant/include/db.hrl ../../misc/include/ip.hrl ../../misc/include/logger.hrl

# Misc

../ebin/$(APP_FILE): $(APP_FILE).src
	sed -e 's;%VSN%;$(MASTER_VSN);' $? > $@
