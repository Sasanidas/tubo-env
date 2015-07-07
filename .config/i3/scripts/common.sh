#!/bin/bash
#
# Author: Yang,Ying-chao <yingchao.yang@icloud.com>, 2019-08-19
#
DEBUG=

die() {
    set +x
    local IFS=$' \t\n'
    set +e

    echo -e "$@"
    exit 1
}

function debug ()
{
    if [ -n "$DEBUG" ]; then
        echo "$@"
    fi
}

DISPLAY_RES_RE="([0-9]+x[0-9]+)\\+([0-9]+)\\+([0-9]+)"

INTERNAL_INFO=`xrandr | grep -e " connected primary*\+"`
INTERNAL_DEV=`echo $INTERNAL_INFO | awk '{print $1}'`
INTERNAL_RES=

if [[ $INTERNAL_INFO =~ $DISPLAY_RES_RE ]]; then
    INTERNAL_RES=${BASH_REMATCH[1]}
fi

EXTERNAL_INFO=`xrandr | grep -sw 'connected' | grep -v "primary"`
EXTERNAL_DEV=`echo $EXTERNAL_INFO | awk '{print $1}'`
EXTERNAL_RES=
if [[ "$EXTERNAL_INFO" =~ $DISPLAY_RES_RE ]]; then
    EXTERNAL_RES=${BASH_REMATCH[1]}
fi
