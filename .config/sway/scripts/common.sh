#!/bin/bash
#
# Author: Yang,Ying-chao <yingchao.yang@icloud.com>, 2019-08-19
#
DEBUG=

die() {
    set +x
    local IFS=$' \t\n'
    set +e

    echo -e "$@" | while read -r ; do
        echo " $BAD*$NORMAL $RC_INDENTATION$REPLY" >&2
    done
    exit 1
}

function debug ()
{
    if [ -n "$DEBUG" ]; then
        echo "$@"
    fi
}
