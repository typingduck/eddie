#!/bin/sh
# Created:  8 Apr 1999 by tobbe@eddieware.org
# Function: Create/Modify the ~/.erlang file.
#           Run this program from the eddie root directory.
#
# Modified: 19990408 geoff@eddieware.org - made more flexible
# Modified: 19990924 geoff@eddieware.org - support separate package root
#
# $Id: mk_dot_erlang.sh,v 1.1 2000/10/27 22:20:27 dredd Exp $
#
# Given a root path ($1) with a lib subdirectory constructs
# a .erlang file (on stdout) for Eddie (or whatever)
#
# An install root now maybe given as a 2nd arg ($2); useful for
# package building.
#
. vsn.mk

if [ x$2 = x ]; then
    find "$1/lib" -follow -name 'ebin' -print | grep $EDDIE_VSN | sed -e 's/\(.*\)/code:add_path(\"\1\")\./' 
else
    find "$2$1/lib" -follow -name 'ebin' -print | grep $EDDIE_VSN | sed -e "s+$2++" | sed -e 's/\(.*\)/code:add_path(\"\1\")\./' 
fi

# Add in the startup command.

echo 'oam:daemon().'

#echo "Your ~/.erlang file now contains this:"
#echo "------------------------------------------"
#cat ~/.erlang
#echo "------------------------------------------"

