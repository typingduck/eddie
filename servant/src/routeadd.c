/*
The contents of this file are subject to the Erlang Public License,
Version 1.0, (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at
http://www.eddieware.org/EPL

Software distributed under the License is distributed on an "AS IS"
basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
the License for the specific language governing rights and limitations
under the License.

The Original Code is Eddie-0.83b1.

The Initial Developer of the Original Code is Ericsson Telecom
AB. Portions created by Ericsson are Copyright (C), 1998, Ericsson
Telecom AB. All Rights Reserved.

Contributor(s): ______________________________________.
Mar 99 - maurice@eddieware.org - Fixes for Solaris/FreeBSD/Linux
*/

#include "config.h"

#include <unistd.h>
#include <stdio.h>
#include <syslog.h>
#include "strs.h"
#include "memfs.h"

#include "rights.h"

static char *cpu_vendor_os = CPU_VENDOR_OS;

int main(int argc,char *argv[]) 
{
	char *interface;
	char *pif;
	char *alias;
	char *ip;
	
	if(argc != 4) 
	{
		fprintf(stderr,"Usage: routeadd interface alias ip-address\n");
		exit(2);
	}

	syslog(LOG_INFO, "Eddie: %s %s %s %s", argv[0], argv[1], argv[2], argv[3]);

	check_rights(argv[0]);

	pif = argv[1];
	alias = argv[2];
	ip = argv[3];

#if defined(LINUX)
	if((interface=(char *)malloc(strlen(pif)+strlen(alias)+2)) == NULL) {
		fprintf(stderr,"routeadd: Out of memory");
		exit(2);
	}
	
	strcpy(interface,pif);
	strcat(interface,":");
	strcat(interface,alias);
	
	/* This stinks! It should be done using ioctl calls. */
	
	if(execl(ROUTE,ROUTE,"add","-host",ip,interface,NULL) == -1) 
	{
		perror("routeadd");
		exit(2);
	}
	
	free(interface);
#elif defined(BSD)
	/* freebsd does not need a route added */
#elif defined(SOLARIS)
	/* solaris does not need a route added */
#elif
# error "routeadd action not defined"
#endif
	return(0);
}
