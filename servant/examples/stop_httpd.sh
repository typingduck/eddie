#!/bin/sh

# $1 = ip address
# $2 = port number

kill `cat /var/tmp/httpd-$1:$2.pid` 2>&1
