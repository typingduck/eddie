#!/bin/sh

# $1 = email address
# $2 = ip address
# $3 = port
# $4 = reason

mail -s "Beware: $2:$3" $1 <<EOF
$4
EOF
