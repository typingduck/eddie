# --------------------------------------------------------------------
# Author  : geoff@eddieware.org
# Modified: dwyer@eddieware.org
# Created : Mar 1999
#
# The contents of this file are subject to the Erlang Public License
# License, Version 1.0, (the "License"); you may not use this file
# except in compliance with the License. You may obtain a copy of the
# License at http://www.eddieware.org/EPL
#
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
#
# The Initial Developer of the Original Code is Ericsson Telecom
# AB. Portions created by Ericsson are Copyright (C), 1998,1999 Ericsson
# Telecom AB. All Rights Reserved.
#
# $Id: Makefile,v 1.1 2000/10/27 22:20:26 dredd Exp $ 
# $Source: /cvsroot/eddie/eddie/oam/src/Makefile,v $ 
# $Revision: 1.1 $ 
# $Date: 2000/10/27 22:20:26 $ 
# $State: Exp $ 
# --------------------------------------------------------------------
# Notes:
#   . Requires GNU Make
#

EDDIE_ROOT=../..

include $(EDDIE_ROOT)/eddie.mk

include $(EDDIE_ROOT)/vsn.mk

# Basic "erl" options used everywhere

ERL_OPTS=+B -noinput -kernel check_ip false
#ERL_OPTS=+B -noinput -kernel check_ip true
BOOT_OPTS=-boot $$INSTALL_ROOT/bin/eddie -config $$INSTALL_ROOT/bin/sys -boot_var INSTALL_ROOT $$INSTALL_LIB
LOADINFO_BOOT=-boot $$INSTALL_ROOT/bin/loadinfo -config $$INSTALL_ROOT/bin/sys -boot_var INSTALL_ROOT $$INSTALL_LIB
#BOOT_OPTS=-boot $RELDIR/$VSN/start -config $RELDIR/$VSN/sys

APP_FILE=oam.app

#MODULES=oam oam_config
MODULES=oam oam_config oam_config_files 

EBIN_FILES=$(MODULES:%=../ebin/%.$(EMULATOR)) ../ebin/$(APP_FILE)

SCRIPT_SRC=$(shell echo *.src)

# Not great - but probably "ok" assuming each attempt to 
# connect to a node will take noticeable time
RANDOM=$(shell date '+%a%S%M%H')

PRIV_FILES := $(SCRIPT_SRC:%.src=../priv/%)

../priv/%: %.src
	sed -e 's;%EDDIE_RSH%;$(EDDIE_RSH);' \
	    -e 's;%EDDIE_VSN%;$(EDDIE_VSN);' \
	    -e 's;%ERL_OPTS%;$(ERL_OPTS);' \
	    -e 's;%ERL_CALL%;$(ERL_CALL);' \
	    -e 's;%BOOT_OPTS%;$(BOOT_OPTS);' \
	    -e 's;%LOADINFO_BOOT%;$(LOADINFO_BOOT);' \
	    -e 's;%ERL_ROOT%;$(ERLDIR);' \
	    -e 's;%LIMIT%;$(LIMIT);' \
	    -e 's;%LOGDIR%;$(LOGDIR);' \
	    -e 's;%SBINDIR%;$(sbindir);' \
	    -e 's;%QUOTE%;$(QUOTE);g' \
	    -e 's;%DB_QUOTE%;$(DB_QUOTE);g' \
        -e 's;%INSTALL_PREFIX%;$(prefix);g' \
        -e 's;%SYSCONFDIR%;$(sysconfdir);g' \
        -e 's;%INSTALL_ROOT%;$(INSTALL_ROOT);g' \
        -e 's;%LOGDIR%;$(LOGDIR);g' \
        -e 's;%DOCDIR%;$(DOCDIR);g' \
        -e 's;%PRODUCT_NAME%;$(PRODUCT_NAME);g' \
        -e 's;%PRODUCT_CNAME%;$(PRODUCT_CNAME);g' \
        -e 's;%RANDOM%;$(RANDOM);g' \
        -e 's;%VERSION%;$(EDDIE_VSN);g' \
        -e 's;%MAJORVERSION%;$(MAJOR_VSN);g' \
        -e 's;%WHOAMI%;$(WHOAMI);g' \
	    -e 's;%HEART_OPEN_QUOTE%;$(HEART_OPEN_QUOTE);g' \
	    -e 's;%HEART_CLOSE_QUOTE%;$(HEART_CLOSE_QUOTE);g' \
	    -e 's;%EDDIE_USER%;$(EDDIE_USER);g' \
	    -e 's;%ROOTDIR%;$(INSTALL_ROOT);' $< > $@
	chmod 775 $@

all: scripts $(EBIN_FILES)

scripts: $(PRIV_FILES) force
	echo Scripts built

clean:
	rm -f $(EBIN_FILES) $(PRIV_FILES)

# Erlang

../ebin/oam.$(EMULATOR): ../../misc/include/logger.hrl

# Misc

../ebin/$(APP_FILE): $(APP_FILE).src
	sed -e 's;%VSN%;$(OAM_VSN);' $? > $@


# Force!
force: ;
