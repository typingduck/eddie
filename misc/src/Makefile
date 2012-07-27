# Variable to be provided
EDDIE_ROOT=../..

include $(EDDIE_ROOT)/eddie.mk

include $(EDDIE_ROOT)/vsn.mk


ifeq ($(findstring cygwin,$(TARGET_OS)), cygwin)
MODULES=config_file db disk_log_handler sig_handler misc sig_ex \
	syslog_handler misc_app misc_sup log_sig
NT=nt.
else
MODULES=config_file db disk_log_handler sig_handler misc sig_ex \
	syslog_handler fdsrv misc_app misc_sup log_sig
NT=
endif

APP_FILE=misc.app

PRIV_FILES=

EBIN_FILES=$(MODULES:%=../ebin/%.$(EMULATOR)) ../ebin/$(APP_FILE)

all: $(EBIN_FILES) $(PRIV_FILES) 

clean:
	rm -f $(EBIN_FILES) $(PRIV_FILES)

# Erlang

../ebin/db.$(EMULATOR): ../../misc/include/logger.hrl

../ebin/disk_log_handler.$(EMULATOR): ../../misc/include/logger.hrl

../ebin/sig_ex.$(EMULATOR): ../../misc/include/eddie_sig.hrl

# Misc


../ebin/$(APP_FILE): $(NT)$(APP_FILE).src
	sed -e 's;%VSN%;$(MISC_VSN);' $? > $@
