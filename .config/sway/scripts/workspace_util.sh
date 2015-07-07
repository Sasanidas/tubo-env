#!/bin/bash
#
# Author: Yang,Ying-chao <yangyingchao@gmail.com>, 2018-03-22
#

set -e

workspaces=`swaymsg -r -t get_workspaces |   tr "\n" " " | sed -E "s/ +//g"`
cur=`echo $workspaces | tr } "\n" | grep '"focused":true' | tr , "\n" |grep num | awk -F ":" '{print $2}'`

debug=

function dlog ()
{
    if [ -n "$debug" ]; then
        echo $@
    fi
}

function exec_i3 ()
{
    dlog "$@"

    eval "$@"

    # polybar-msg hook focused_window 1

    if [ -n "$debug" ]; then
        notify-send -t 600 "$@"
    fi

    # # Issue mouse click..
    # screen_res=`xrandr | grep -e " connected.*\+" |sed "s/ /\n/g" | grep -e ".*x.*+.*+.*" | awk -F "+" '{print $1}'`
    # xpos=`echo $screen_res | awk -F "x" '{print $1/2}'`
    # ypos=`echo $screen_res | awk -F "x" '{print $2/2}'`

    # xdotool mousemove $xpos $ypos
    # xdotool click 1
}

declare -a wks # all available workspaces.
slot=0

for wk in `echo $workspaces  | tr , "\n" |grep num | awk -F ":" '{print $2}'`; do
    dlog "Workspace: $wk"

    wks[$slot]=$wk

    if [ $wk -eq $cur ]; then
        cur_slot=$slot
    fi

    slot=$((slot+1))
done

total_slots=$((slot-1))

dlog "First: $first, Last: $last, Cur: $cur, ALL: ${wks[@]}"


case $1 in
    go_right)
         next=$((cur_slot+1))
         dlog  "NEXT: ${next}, ALL${wks[*]}"

        if [ $next -eq ${#wks[@]} ]; then
            next=0
            dlog "Wrap to 0: ${wks[$next]}"
        fi
        exec_i3 swaymsg "workspace ${wks[$next]}"
        ;;
    go_left)
        next=$((cur_slot-1))
        exec_i3 swaymsg "workspace ${wks[$next]}"
        ;;
    go_first)
        exec_i3 swaymsg "workspace ${wks[0]}"
        ;;
    go_last)
       exec_i3 swaymsg "workspace ${wks[-1]}"
        ;;
    new)
        next=0
        for i in `seq 1 9`; do
            for j in `seq 0 ${total_slots}`; do
                exist=0
                if [ $i -eq ${wks[$j]}  ]; then
                    exist=1
                    break
                fi
            done

            if [ $exist -eq 0 ]; then
                next=$i
                break
            fi
        done
        exec_i3 swaymsg "workspace $next"
        ;;
    *)
        echo "Usage: $0 go_right|go_left|go_first|go_last|new"
    ;;
esac
