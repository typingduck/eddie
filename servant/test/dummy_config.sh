#!/bin/sh
/sbin/ifconfig dummy 150.236.14.156 up
/sbin/route add -host 150.236.14.156 dummy
