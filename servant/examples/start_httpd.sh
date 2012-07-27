#!/bin/sh

# $1 = ip address
# $2 = port
# $3 = httpd
# $4 = config dir (must be an absolute path!)

if [ -f /var/tmp/httpd-$1:$2.pid ]; then
    kill `cat /var/tmp/httpd-$1:$2.pid` > /dev/null 2>&1
fi

sed -e "s/@IP/$1/g" -e "s/@PORT/$2/g" $4/httpd.conf > $4/httpd-$1:$2.conf

$3 -f $4/httpd-$1:$2.conf 2>&1
