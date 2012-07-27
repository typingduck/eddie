#!/bin/sh

# $1 = ip address
# $2 = port
# $3 = poll interval (secs)

# This is just a dummy monitor! It does not monitor *anything* but
# rather shows the type of output a monitor should return.

n=0

while [ $n -lt 5 ]; do
    n=`expr $n + 1`
    sleep $3
    printf "ok"
done

printf "terminated"
