# Variable to be provided
EDDIE_ROOT = ../..

include $(EDDIE_ROOT)/eddie.mk

MODULES=dummy eddie http_monitor

FILES=$(MODULES:%=../ebin/%.$(EMULATOR))

PGMS=start_httpd stop_httpd

all: $(FILES) $(PGMS)

clean:
	rm -f $(FILES) $(PGMS)

# C

start_httpd: start_httpd.c
	$(CC) -Wall -ansi $< -o $@

stop_httpd: stop_httpd.c
	$(CC) -Wall -ansi $< -o $@
