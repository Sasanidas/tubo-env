#!/bin/bash
#
# Author: Yang,Ying-chao <yingchao.yang@icloud.com>, 2019-08-12
#

die() {
    notify-send "$@"
    exit 1
}

function do_scrot ()
{
    scrot "$@" ${HOME}/tmp/'screenshot_%Y-%m-%d_%H:%M:%S'.png -e 'notify-send "$f saved.."'
}

which scrot >/dev/null 2>&1 || die  "Can't find scrot..."

if [ $# -eq 0 ]; then
    do_scrot
else
    do_scrot -s
fi
