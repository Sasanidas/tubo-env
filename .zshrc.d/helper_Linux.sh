#!/bin/bash
#
# Author: Yang,Ying-chao <yangyingchao@g-data.com>, 2018-01-19
#

which mimeopen >/dev/null 2>&1 open_cmd="mimeopen -n" ||  open_cmd="xdg-open"

function open ()
{
    ${open_cmd} $@ &
}
