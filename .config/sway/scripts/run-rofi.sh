#!/bin/bash
#
# Author: Yang,Ying-chao <yingchao.yang@icloud.com>, 2019-08-19
#
source ${HOME}/.config/i3/scripts/common.sh

debug "Running rofi: $@"

case $1 in
    run)
        rofi -show run -run-command "urxvt -e {cmd}"
    ;;
    window)
        rofi  -show window -config ~/.config/rofi/themes/Switch.rasi
    ;;
    search)
        rofi  -show Search -modi "Search:/home/yyc/.config/i3/scripts/search.sh"
        ;;
    ssh)
        rofi -show ssh
        ;;
    power)
        ~/.config/rofi/scripts/Powermenu.sh
        ;;
    *)
        rofi -show combi -combi-modi "Search:/home/yyc/.config/i3/scripts/search.sh,drun,run"
    ;;
esac
