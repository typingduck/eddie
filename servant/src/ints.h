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

#ifndef INTS_H

#include "config.h"

#ifndef HAVE_int8_t
typedef BIT8 int8_t;
#endif
#ifndef HAVE_u_int8_t
typedef unsigned BIT8 u_int8_t;
#endif
#ifndef HAVE_int16_t
typedef BIT16 int16_t;
#endif
#ifndef HAVE_u_int16_t 
typedef unsigned BIT16 u_int16_t;
#endif
#ifndef HAVE_int32_t 
typedef BIT32 int32_t;
#endif
#ifndef HAVE_u_int32_t 
typedef unsigned BIT32 u_int32_t;
#endif
#ifndef HAVE_int64_t
typedef BIT64 int64_t;
#endif
#ifndef HAVE_u_int64_t
typedef unsigned BIT64 u_int64_t;
#endif

#ifdef NEED_SYS_TYPES_H
# include <sys/types.h>
#endif
#ifdef NEED_SYS_BITYPES_H
# include <sys/bitypes.h>
#endif

#define INTS_H 1
#endif
