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
*/

#include <stdio.h>
#include <stdarg.h>

#include "config.h"

static char *cpu_vendor_os = CPU_VENDOR_OS;

void error(int value,char *formatString,...) 
{
	va_list ap;

	va_start(ap,formatString);
	vfprintf(stderr,formatString,ap);
	va_end(ap);
	exit(value);
}
