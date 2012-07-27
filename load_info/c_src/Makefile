# Variable to be provided
EDDIE_ROOT = ../..

include $(EDDIE_ROOT)/eddie.mk

BSD_LOAD_AVG=../priv/load_avg
NT_TARGET = ../priv/NTLoadCounter.exe

LIBS   += -lm

all: 
	$(MAKE) $(TARGET_OS)

bsd: $(BSD_LOAD_AVG)
	chmod 775 $(BSD_LOAD_AVG)

$(BSD_LOAD_AVG): load_avg.c
	$(CC) -o $@ $(CFLAGS) load_avg.c $(LIBS)

linux:

solaris:

# Windows NT. Will need something here eventually.
cygwin: $(NT_TARGET)

$(NT_TARGET): NTLoadCounter.c
	 cl.exe /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" $< advapi32.lib /link /OUT:$(NT_TARGET)

clean:
	rm -f *.o *~ $(BSD_LOAD_AVG)
	rm -f *.obj $(NT_TARGET)
