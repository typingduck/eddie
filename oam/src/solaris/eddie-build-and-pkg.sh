#!/usr/local/bin/bash
#
# This script builds eddie and then packages it as a solaris
#   package that can be installed with pkgadd
#
# Author: Jon Holdsworth jon@central.warehouse.net
#  IP:    (c) Ericsson Australia, 2000
#
#
# debug options
# -v = print command lines as they are read in.
# -n = read them in but don't execute them.
# -x = more full-on version of -v, but only works
#      if commands are actually executed, hence mutually
#      exclusive with -n.

# this script builds and packages eddie
# under solaris 2.7

# it must be accompanied by the files described
#   by the variables:

#     $EDDIE_REQUEST
#     $EDDIE_POSTINSTALL
#     $EDDIE_PKGINFO
#     $EDDIE_DEPEND

# in the same directory as this script.


# it must be run as root

# a revision number must be supplied onthe command line

# at writing it cannot checkout eddie because
# none of the solaris machines have direct access to
# the CVS repository.

# the eddie source tree once checked out must
# be placed in EDDIE_SOURCE_DIR.


# command line parameters:

# eddie-build-and-pkg.sh  <product> <version> <revision string>
#


# all variables should go here

#SCRIPT_NAME=eddie-build-and-pkg.sh
SCRIPT_NAME=$0

PRODUCT=$1
VERSION=$2
EDDIE_REVISION_STRING=$3

# set the form of the solaris package instance name (its symbolic name)
BASE_EDDIE_PACKAGE_NAME=eddie

# temporary for safety
EDDIE_PACKAGE_NAME=$BASE_EDDIE_PACKAGE_NAME

# packages that must be present for eddie to compile
ERLANG_PACKAGE_NAME=erlang491
OPENSSL_PACKAGE_NAME=opnssl093
PRE_REQ_PACKAGES="$ERLANG_PACKAGE_NAME $OPENSSL_PACKAGE_NAME"

# internal use
# no leading slash!
REL_EDDIE_USR_ROOT=usr/local
REL_EDDIE_VAR_ROOT=var
REL_EDDIE_ETC_ROOT=etc


# note this must come BEFORE any variables that use it !!
STARTING_DIR=`pwd`

# where source comes from
EDDIE_SOURCE_DIR=$STARTING_DIR/${PRODUCT}-${VERSION}

# where in the source to get the version from
EDDIE_VERSION_FILE=$EDDIE_SOURCE_DIR/vsn.mk

# where the installed result goes to for staging and then packaging
ROOT=$STARTING_DIR/test_pit

EDDIE_PKGINFO=$STARTING_DIR/eddie-pkginfo
EDDIE_REQUEST=$STARTING_DIR/eddie-request
EDDIE_POSTINSTALL=$STARTING_DIR/eddie-postinstall
EDDIE_DEPEND=$STARTING_DIR/eddie-depend


# banner

echo
echo Starting - $SCRIPT_NAME
echo
echo "Remember this needs package(s) [ $PRE_REQ_PACKAGES ] installed in order to compile !!"
echo


# where are we

echo
echo "Starting in [ "$STARTING_DIR" ]"
echo


# check root user

if [ x`whoami` != "xroot" ] ; then
  echo "Error: Must run as root!"
  exit
fi
echo 
echo Running as root - is correct
echo


# revision string

if [ x"$EDDIE_REVISION_STRING" == x ] ; then
  echo
  echo "Error: no Revision string supplied!"
  echo
  echo "Usage: ./`basename $0` <product> <version> <revision string>"

  exit

#  EDDIE_REVISION_STRING="0"
fi

echo
echo "Eddie Revision string now [ $EDDIE_REVISION_STRING ]"
echo


# check for Erlang

if  pkginfo -q $ERLANG_PACKAGE_NAME ; then
  echo
  echo "Erlang package detected ($ERLANG_PACKAGE_NAME)" ; 
  echo
else
  echo
  echo "Error: No Erlang package detected ($ERLANG_PACKAGE_NAME) !"
  echo "Halting"
  echo
  exit 
fi

# check for OpenSSL

if  pkginfo -q $OPENSSL_PACKAGE_NAME ; then
  echo
  echo "OpenSSL package detected ($OPENSSL_PACKAGE_NAME)" ; 
  echo
else
  echo
  echo "Error: No OpenSSL package detected ($OPENSSL_PACKAGE_NAME) !"
  echo "Halting"
  echo
  exit 
fi


# check support files are present

if [ ! -f "$EDDIE_PKGINFO" ] ; then
   echo "Error: $EDDIE_PKGINFO is not present !"
   exit
fi
echo "$EDDIE_PKGINFO is present"

if [ ! -f "$EDDIE_REQUEST" ] ; then
   echo "Error: $EDDIE_REQUEST is not present !"
   exit
fi
echo "$EDDIE_REQUEST is present"

if [ ! -f "$EDDIE_POSTINSTALL" ] ; then
   echo "Error: $EDDIE_POSTINSTALL is not present !"
   exit
fi
echo "$EDDIE_POSTINSTALL is present"

if [ ! -f "$EDDIE_DEPEND" ] ; then
   echo "Error: $EDDIE_DEPEND is not present !"
   exit
fi
echo "$EDDIE_DEPEND is present"




# check source directory exists

if [ ! -d "$EDDIE_SOURCE_DIR" ]; then
   echo "Error: EDDIE_SOURCE_DIR does not exist ! [ $EDDIE_SOURCE_DIR ]"
   exit
fi
echo "Found EDDIE_SOURCE_DIR [ $EDDIE_SOURCE_DIR ]"



# setup the environment
# ---------------------

ulimit -n 1024
echo ulimit -n set to `ulimit -n`


# do a make clean - may fail
cd $EDDIE_SOURCE_DIR
make clean


if [ ! -f $EDDIE_VERSION_FILE ] ; then
   echo
   echo "Error: Version file not present [ $EDDIE_VERSION_FILE ] !"
   echo "Halting"
   echo
   exit
fi

# extract Eddie VERSION information
EDDIE_VSN=`grep "^EDDIE_VSN"                $EDDIE_VERSION_FILE | awk '{ FS="=" ; print $2 }'`

# make a VERSION string using the extracted version info and the supplied Revision number
EDDIE_VERSION_STRING="${EDDIE_VSN}-${EDDIE_REVISION_STRING}"

echo
echo "Eddie version string found from [ $EDDIE_VERSION_FILE ] and Revision [ $EDDIE_REVISION_STRING ] as [ $EDDIE_VERSION_STRING ]"
echo

# use VERSION information to build package symbolic name
SHORT_EDDIE_VERSION_STRING=`echo $EDDIE_VSN | awk '{ FS="." ; print $1$2$3 }'`

EDDIE_PACKAGE_NAME=$BASE_EDDIE_PACKAGE_NAME"$SHORT_EDDIE_VERSION_STRING"

echo
echo "Eddie package symbolic name now [ $EDDIE_PACKAGE_NAME ]"
echo



#####################
### BUILDING ZONE ###
#####################


# enter the build directory
# -------------------------

cd $EDDIE_SOURCE_DIR

echo
echo "Now in [ `pwd` ]"
echo


# run the configure script
# ------------------------

echo
echo "Calling configure"
echo

./configure



# we used to: have to do a couple of minor modifications
# to fix some sun static library problems before we
# can compile.


# fix eddie.mk

#echo
#echo Fixing eddie.mk
#echo

#LD_SHOULD_BE="LDFLAGS= -lsocket -lnsl -L/usr/lib/libnsl.so.1"
#LIBS_SHOULD_BE="LIBS=-lsocket -lnsl -L/usr/lib/libnsl.so.1"

#cat eddie.mk | awk "!/LDFLAGS=/ { print } ; /LDFLAGS=/ { print \"$LD_SHOULD_BE\" } " > /tmp/eddie.mk.tmp1
#cat /tmp/eddie.mk.tmp1 | awk "!/LIBS=/ { print } ; /LIBS=/ { print \"$LIBS_SHOULD_BE\" } "> /tmp/eddie.mk.tmp2
#rm eddie.mk
#mv /tmp/eddie.mk.tmp2 eddie.mk
#rm /tmp/eddie.mk.tmp1


# remove '-static' directive in inet_server/c_src/Makefile

#echo
#echo Fixing $EDDIE_SOURCE_DIR/inet_server/c_src/Makefile
#echo

#cd $EDDIE_SOURCE_DIR/inet_server/c_src

#cat Makefile | awk "!/static/ { print } ; /static/ { print \"\t\$(CC) -o \$@ \$(LDFLAGS) \$(BASE_OBJ) \$(SSL_OBJ) \$(LIBS) \$(SSL_LIBS)\" }" > /tmp/Makefile.tmp

#rm Makefile
#mv /tmp/Makefile.tmp Makefile

cd $EDDIE_SOURCE_DIR



# start the build

echo
echo "Calling make"
echo

make

echo
echo Eddie built
echo



######################
#### STAGING ZONE ####
######################


# set names of staging and packaging directories respectively
EDDIE_STAGE_DIR=$ROOT/eddie"$SHORT_EDDIE_VERSION_STRING".staging
EDDIE_PKG_DIR=$ROOT/eddie"$SHORT_EDDIE_VERSION_STRING".package


# remove any previous effort

echo
echo  Removing $EDDIE_STAGE_DIR and $EDDIE_PKG_DIR
echo

rm -rf $EDDIE_STAGE_DIR $EDDIE_PKG_DIR



echo
echo Calling make install root=$EDDIE_STAGE_DIR
echo

make install root=$EDDIE_STAGE_DIR


# put in any extras

echo
echo "Copying eddie.setboot script -> $EDDIE_STAGE_DIR/usr/local/lib/eddie/doc/"
echo
cp $EDDIE_SOURCE_DIR/oam/src/solaris/eddie.setboot  $EDDIE_STAGE_DIR/usr/local/lib/eddie/doc/
chown root  $EDDIE_STAGE_DIR/usr/local/lib/eddie/doc/eddie.setboot
chgrp other $EDDIE_STAGE_DIR/usr/local/lib/eddie/doc/eddie.setboot
chmod 700   $EDDIE_STAGE_DIR/usr/local/lib/eddie/doc/eddie.setboot

echo
echo "Copying lbdns.setboot script -> $EDDIE_STAGE_DIR/usr/local/lib/eddie/doc/"
echo
cp $EDDIE_SOURCE_DIR/oam/src/solaris/lbdns.setboot  $EDDIE_STAGE_DIR/usr/local/lib/eddie/doc/
chown root  $EDDIE_STAGE_DIR/usr/local/lib/eddie/doc/lbdns.setboot
chgrp other $EDDIE_STAGE_DIR/usr/local/lib/eddie/doc/lbdns.setboot
chmod 700   $EDDIE_STAGE_DIR/usr/local/lib/eddie/doc/lbdns.setboot


# ok, now eddie is correctly arranged under the staging directory

echo
echo Setting up boot scripts...
echo

# now set up the boot scripts

mkdir -p $EDDIE_STAGE_DIR/etc/init.d


# set owner and group to solaris standard

chown root $EDDIE_STAGE_DIR/etc/  $EDDIE_STAGE_DIR/etc/*
chgrp sys  $EDDIE_STAGE_DIR/etc/  $EDDIE_STAGE_DIR/etc/*


# set permissions to solaris standard

chmod 755  $EDDIE_STAGE_DIR/etc/
chmod 775  $EDDIE_STAGE_DIR/etc/*


# copy in our boot scripts, performing any substitutions we have
# to make:

# eddie.init script

cat $EDDIE_SOURCE_DIR/oam/src/solaris/eddie.init.solaris.src | sed -e "s:%SBINDIR%:/usr/local/sbin:" -e "s:%PRODUCT_NAME%:eddie:" > $EDDIE_STAGE_DIR/etc/init.d/eddie.init

# lbdns.init script

cat $EDDIE_SOURCE_DIR/oam/src/solaris/lbdns.init.solaris.src | sed -e "s:%SBINDIR%:/usr/local/sbin:" -e "s:%PRODUCT_NAME%:eddie:" > $EDDIE_STAGE_DIR/etc/init.d/lbdns.init

# set owner group and permissions

cd $EDDIE_STAGE_DIR/etc/init.d
chown root *
chgrp sys *
chmod 744 *

# note: the symlinks are added or removed by another script at the user's discretion

echo
echo Boot scripts set up
echo


######################
### PACKAGING ZONE ###
######################


# create the packaging directory

mkdir -p $EDDIE_PKG_DIR
echo
echo "Packaging directory created [ $EDDIE_PKG_DIR ]."


# change to the staging directory

cd $EDDIE_STAGE_DIR

echo
echo "Now in [ `pwd` ]."
echo


# install the packaging system files


# insert version into pkginfo file
#cp $EDDIE_PKGINFO ./pkginfo
cat $EDDIE_PKGINFO | awk "!/VERSION=/ { print } ; /VERSION=/ { print \"VERSION=V $EDDIE_VERSION_STRING \" } " > ./pkginfo.tmp
cat ./pkginfo.tmp | awk "!/PKG=/ { print } ; /PKG=/ { print \"PKG=$EDDIE_PACKAGE_NAME \" } " > ./pkginfo
rm -f ./pkginfo.tmp

# insert details into request file
#cp $EDDIE_REQUEST ./request
cat $EDDIE_REQUEST | sed -e "s:%ERLANG_PACKAGE_NAME%:$ERLANG_PACKAGE_NAME:" -e "s:%REL_EDDIE_USR_ROOT%:$REL_EDDIE_USR_ROOT:" -e "s:%REL_EDDIE_VAR_ROOT%:$REL_EDDIE_VAR_ROOT:"  -e "s:%REL_EDDIE_ETC_ROOT%:$REL_EDDIE_ETC_ROOT:" > ./request

# postinstall file
cp $EDDIE_POSTINSTALL ./postinstall

# depend file
cat $EDDIE_DEPEND | sed -e "s:%ERLANG_PACKAGE_NAME%:$ERLANG_PACKAGE_NAME:" > ./depend

echo
echo Packaging system files installed
echo



# permissions should be taken care of by the Makefile so
#  this following chunk has been commented out for now:

#echo 
#echo Setting permissions
#echo

# make sure root owns+groups everything

#find . | xargs chown root
#find . | xargs chgrp root

# change some specific files and directories to specific settings

#chgrp eddie usr/local/etc/eddie.conf
#chmod 640 usr/local/etc/eddie.conf

#chmod 755 usr/local/etc

#chgrp eddie var/eddie
#chmod 770 var/eddie


# build the prototype file

echo
echo Creating prototype file
echo

find etc/ usr/ var/  -print | pkgproto -c eddie $REL_EDDIE_USR_ROOT=\$EDDIE_USR_ROOT $REL_EDDIE_VAR_ROOT=\$EDDIE_VAR_ROOT $REL_EDDIE_ETC_ROOT=\$EDDIE_ETC_ROOT > ../prototype

echo "i pkginfo"       >  ./prototype
echo "i request"       >> ./prototype
echo "i postinstall"   >> ./prototype
echo "i depend"        >> ./prototype

cat ../prototype >> ./prototype

rm ../prototype


# build the package 

echo
echo "Creating package [ $EDDIE_PACKAGE_NAME ] in [ $EDDIE_PKG_DIR ]."
echo

pkgmk -d $EDDIE_PKG_DIR  $EDDIE_PACKAGE_NAME

echo
echo "Package [ $EDDIE_PACKAGE_NAME ] created (in directory format)"
echo


# build the streamed version of the package 

EDDIE_STREAM_PACKAGE_NAME="eddie-$EDDIE_VSN-$EDDIE_REVISION_STRING.sparc-sun-solaris2.7.pkg"

echo
echo "Creating streamed package file [ $EDDIE_STREAM_PACKAGE_NAME ] for $EDDIE_PACKAGE_NAME in [ $EDDIE_PKG_DIR ]."
echo

pkgtrans -os $EDDIE_PKG_DIR  $EDDIE_STREAM_PACKAGE_NAME  $EDDIE_PACKAGE_NAME

echo
echo "Package [ $EDDIE_PACKAGE_NAME ] created (in stream-file format)"
echo


# all done
# --------

echo
echo "Done ( $SCRIPT_NAME )."
echo

# end of file
