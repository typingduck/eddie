#include <stdio.h>
#include <stdarg.h>

#include "param.h"
#include "control.h"

void fatal(char *msg, ...)
{
    va_list param_list;

    va_start(param_list, msg);
    vfprintf(stderr, msg, param_list);
    exit(1);
}

int main()
{
    /* sanity checking */
    if (sizeof(u16) != 2)
	fatal("typedef for u16 does not result in a 16-bit int\n");
    if (sizeof(u32) != 4)
	fatal("typedef for u32 does not result in a 32-bit int\n");
    if (MAX_CHILDREN + 4 > MAX_FD)
	fatal("error - MAX_FD (= %d) is not large enough to "
		"accommodate MAX_CHILDREN (= %d)\n", MAX_FD, MAX_CHILDREN);
    if (6 + 2*MAX_CONNECTIONS > MAX_FD)
	fatal("error - MAX_FD (= %d) is not large enough to "
		"accommodate MAX_CONNECTIONS (= %d)\n", MAX_FD,
		MAX_CONNECTIONS);
#ifdef SYSV
    if (MAX_FD > FD_SETSIZE)
	fatal("MAX_FD (= %d) set larger than available FDs (= %d)\n",
		MAX_FD, FD_SETSIZE);
#else
    if (MAX_FD > getdtablesize())
	fatal("MAX_FD (= %d) set larger than available FDs (= %d)\n",
		MAX_FD, getdtablesize());
#endif
    exit(0);
}
