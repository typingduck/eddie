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
### Author: Magnus Fröberg
### Contributor(s): 
### 990325 - tobbe@eddieware.org
### 990401 - geoff@eddieware.org - substitution updates
###
### $Id: Makefile,v 1.1 2000/10/27 22:20:23 dredd Exp $ 
### $Source: /cvsroot/eddie/eddie/dns_server/system/Makefile,v $ 
### $Revision: 1.1 $ 
### $Date: 2000/10/27 22:20:23 $ 
### $State: Exp $ 
### --------------------------------------------------------------------

# Variable to be provided
EDDIE_ROOT=../..

include $(EDDIE_ROOT)/eddie.mk

include $(EDDIE_ROOT)/vsn.mk

include system.mk

PREFIX = ../..
PA = -pa ../../dns_server/ebin -pa ../../misc/ebin
DNS_BOOT=-boot $$DNS_ROOT/bin/dns -config $$DNS_ROOT/bin/dns_sys

DOC=../../../doc/dns

all: dns.boot lbdns sys.config 

lbdns: start.src
	sed -e 's;%ROOTDIR%;$(INSTALL_ROOT);' \
        -e 's;%DNS_BOOT%;$(DNS_BOOT);' \
        -e 's;%SBINDIR%;$(sbindir);' \
        -e 's;%QUOTE%;$(QUOTE);g' \
        -e 's;%HEART_OPEN_QUOTE%;$(HEART_OPEN_QUOTE);g' \
        -e 's;%HEART_CLOSE_QUOTE%;$(HEART_CLOSE_QUOTE);g' \
        -e 's;%INSTALL_PREFIX%;$(prefix);g' \
        -e 's;%SYSCONFDIR%;$(sysconfdir);g' \
        -e 's;%VERSION%;$(EDDIE_VSN);g' \
        -e 's;%PRODUCT_NAME%;$(PRODUCT_NAME);g' \
        -e 's;%PRODUCT_CNAME%;$(PRODUCT_CNAME);g' \
	    -e 's;%ERL_ROOT%;$(ERLDIR);' start.src > ../priv/lbdns
	chmod 755 ../priv/lbdns

sys.config: sys.config.src
	sed -e "s;%MF_BYTES%;512000;" \
	    -e "s;%MF_FILES%;5;" sys.config.src > ../priv/dns_sys.config

dns.rel: dns.rel.src
	sed -e 's;%I_DNS_VSN%;$(DNS_SERVER_VSN);' \
	    -e 's;%KERNEL_VSN%;$(KERNEL_VSN);' \
	    -e 's;%STDLIB_VSN%;$(STDLIB_VSN);' \
	    -e 's;%SASL_VSN%;$(SASL_VSN);' \
	    -e 's;%MISC_VSN%;$(MISC_VSN);' \
	    -e 's;%I_VSN%;$(ERTS_VSN);' \
	    -e 's;%CRYPTO_VSN%;$(CRYPTO_VSN);' \
	    -e 's;%I_SYSTEM_VSN%;$(SYSTEM_VSN);' \
          dns.rel.src > dns.rel

dns.boot: dns.rel make.$(EMULATOR) ../ebin
	$(ERL) $(PA) -noshell +B -s make script dns $(EMULATOR) $(PREFIX)
	mv dns.boot ../priv

dns.tar.gz: dns.rel make.$(EMULATOR)
	$(ERL) $(PA) -noshell +B -s make tar dns $(EMULATOR)

clean:
	rm -f dns.rel
	rm -f dns.script
	rm -f ../priv/dns_sys.config
	rm -f ../priv/dns.boot
	rm -f ../priv/lbdns
