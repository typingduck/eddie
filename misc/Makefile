# Variables to be provided
EDDIE_ROOT = ..

include $(EDDIE_ROOT)/eddie.mk

all:
	cd src;$(MAKE) $(EDDIE_VARS)
	cd c_src;$(MAKE) $(EDDIE_VARS) $(TARGET_OS)

clean:
	cd src;$(MAKE) $(EDDIE_VARS) clean
	cd c_src;$(MAKE) $(EDDIE_VARS) clean
