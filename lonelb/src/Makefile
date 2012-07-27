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
###     Aug 99 Geoff Wong
###
### $Id: Makefile,v 1.1 2000/10/27 22:20:26 dredd Exp $ 
### $Source: /cvsroot/eddie/eddie/lonelb/src/Makefile,v $ 
### $Revision: 1.1 $ 
### $Date: 2000/10/27 22:20:26 $ 
### $State: Exp $ 
### --------------------------------------------------------------------

EDDIE_ROOT = ../..

include $(EDDIE_ROOT)/eddie.mk

include $(EDDIE_ROOT)/vsn.mk

APP_FILES= lonelb

MODULES= get_mig_conf 

EBIN_FILES=$(MODULES:%=../ebin/%.$(EMULATOR)) $(APP_FILES:%=../ebin/%.app)

all: $(EBIN_FILES) 

clean:
	rm -f $(EBIN_FILES) 

# Application file stuff

../ebin/%.app: %.app.src
	sed -e 's;%VSN%;$(SERVANT_VSN);' $? > $@


