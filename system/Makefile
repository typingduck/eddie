
### --------------------------------------------------------------------
### Author: Magnus Fröberg
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
### Contributor(s): ______________________________________.
### Aug 1999 - geoff@eddieware.org - Lone load info support
### Mar 1999 - geoff@eddieware.org - Deleted heaps of stuff
### Apr 1999 - geoff@eddieware.org - Modified boot stuff
### 990322 - tobbe@eddieware.org
###
### $Id: Makefile,v 1.1 2000/10/27 22:20:27 dredd Exp $ 
### $Source: /cvsroot/eddie/eddie/system/Makefile,v $ 
### $Revision: 1.1 $ 
### $Date: 2000/10/27 22:20:27 $ 
### $State: Exp $ 
### --------------------------------------------------------------------

#
# This Makefile is intended to allow developers to
# build releases of Eddie including the proprietary OTP stuff.
#

# Variable to be provided
EDDIE_ROOT=..

include $(EDDIE_ROOT)/eddie.mk
include $(EDDIE_ROOT)/vsn.mk
include system.mk

OAM_APP_DIRS = ../oam/ebin ../oam/priv
APP_DIRS = ../load_info/ebin ../inet_server/ebin ../sync_nodes/ebin ../servant/ebin ../master/ebin ../misc/ebin $(OAM_APP_DIRS)

DOC=../../doc/eddie

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
PREFIX = ..

PA = -pa ../load_info/ebin -pa ../inet_server/ebin -pa ../sync_nodes/ebin -pa ../oam/ebin -pa ../misc/ebin -pa ../master/ebin -pa ../servant/ebin -pa ../lonelb/ebin -pa .

all: eddie.boot loadinfo.boot sys.config

#all: eddie-$(SYSTEM_VSN)_$(OS_TARGET).tar.gz

eddie-$(SYSTEM_VSN)_$(OS_TARGET).tar.gz: eddie-$(SYSTEM_VSN)
	@rm -f eddie-$(SYSTEM_VSN)_$(OS_TARGET).tar.gz
	@rm -f eddie-$(SYSTEM_VSN)_$(OS_TARGET).tar
	tar cf eddie-$(SYSTEM_VSN)_$(OS_TARGET).tar eddie-$(SYSTEM_VSN)
	gzip eddie-$(SYSTEM_VSN)_$(OS_TARGET).tar
	cp -p eddie-$(SYSTEM_VSN)_$(OS_TARGET).tar.gz ../releases/.


%.rel: %.rel.src $(EDDIE_ROOT)/vsn.mk
	sed -e 's;%KERNEL_VSN%;$(KERNEL_VSN);' \
	    -e 's;%STDLIB_VSN%;$(STDLIB_VSN);' \
	    -e 's;%SASL_VSN%;$(SASL_VSN);' \
	    -e 's;%OS_MON_VSN%;$(OS_MON_VSN);' \
	    -e 's;%MNESIA_VSN%;$(MNESIA_VSN);' \
	    -e 's;%MNEMOSYNE_VSN%;$(MNEMOSYNE_VSN);' \
	    -e 's;%MISC_VSN%;$(MISC_VSN);' \
	    -e 's;%SYNC_NODES_VSN%;$(SYNC_NODES_VSN);' \
	    -e 's;%LOAD_INFO_VSN%;$(LOAD_INFO_VSN);' \
	    -e 's;%SERVANT_VSN%;$(SERVANT_VSN);' \
	    -e 's;%MASTER_VSN%;$(MASTER_VSN);' \
	    -e 's;%INET_SERVER_VSN%;$(INET_SERVER_VSN);' \
	    -e 's;%DNS_SERVER_VSN%;$(DNS_SERVER_VSN);' \
	    -e 's;%OAM_VSN%;$(OAM_VSN);' \
	    -e 's;%I_VSN%;$(ERTS_VSN);' \
	    -e 's;%I_SYSTEM_VSN%;$(EDDIE_VSN);' \
          $< > $@

eddie.boot: eddie.rel make.$(EMULATOR) $(APP_DIRS)
	$(ERL) $(PA) -noshell +B -s make script eddie $(EMULATOR) $(PREFIX)

loadinfo.boot: loadinfo.rel make.$(EMULATOR) $(APP_DIRS)
	$(ERL) $(PA) -noshell +B -s make script loadinfo $(EMULATOR) $(PREFIX)

eddie_clean.boot: eddie_clean.rel
	$(ERLC) -W $(PA) eddie_clean.rel

ifeq ($(findstring cygwin,$(TARGET_OS)),cygwin)
sys.config: nt.sys.config.src
	sed -e 's;%CPU_SUP%;$(CPU_SUP);' nt.sys.config.src > sys.config
else
sys.config: sys.config.src
	sed -e 's;%CPU_SUP%;$(CPU_SUP);' sys.config.src > sys.config
endif

#sys.config.linux: sys.config.src
#	sed -e 's;%CPU_SUP%;false;' sys.config.src > sys.config.linux	
#
#sys.config.sunos5: sys.config.src
#	sed -e 's;%CPU_SUP%;true;' sys.config.src > sys.config.sunos5	

Install: Install.src
	sed -e 's;%EMULATOR%;$(EMULATOR);' \
	    -e 's;%MISC_VSN%;$(MISC_VSN);' \
	    -e 's;%SYNC_NODES_VSN%;$(SYNC_NODES_VSN);' \
	    -e 's;%LOAD_INFO_VSN%;$(LOAD_INFO_VSN);' \
	    -e 's;%SERVANT_VSN%;$(SERVANT_VSN);' \
	    -e 's;%MASTER_VSN%;$(MASTER_VSN);' \
	    -e 's;%INET_SERVER_VSN%;$(INET_SERVER_VSN);' \
	    -e 's;%OAM_VSN%;$(OAM_VSN);' \
	    -e 's;%I_VSN%;$(ERTS_VSN);' \
	    -e 's;%TARGET%;$(OS_TARGET);' \
	    -e 's;%I_SYSTEM_VSN%;$(SYSTEM_VSN);' \
          Install.src > Install

clean:
	rm -f eddie.rel
	rm -f eddie.boot
	rm -f eddie.script
	rm -f loadinfo.rel
	rm -f loadinfo.boot
	rm -f loadinfo.script
	rm -f sys.config
	rm -f make.$(EMULATOR)
	rm -f Install
	rm -f eddie.tar.gz
	rm -f eddie_clean.rel eddie_clean.boot 

veryclean: clean

