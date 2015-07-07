#!/bin/bash
#
# Author: Yang,Ying-chao <yingchao.yang@icloud.com>, 2019-08-24
#

scriptdir=${0%/*}
source ${scriptdir}/common.sh

[ $UID -eq 0 ] && die "Don't call it as root..."

action=$1

primary_dev="eDP-1"
secondary_dev=`swaymsg -p -t get_outputs  | grep Output | grep -v eDP | awk '{print $2}'`

image_dir=${HOME}/.config/wallpaper

function get_sway_socket ()
{
    for item in `ls -1 /var/run/user/${UID}/sway-ipc.${UID}.*.sock`; do
        echo $item
        return 0
    done

    return 1
}

get_sway_socket

function exec_swaymsg ()
{
    local sock=`get_sway_socket`
    [ -z "$sock" ] && die "Can't find proper socket."
    swaymsg -s ${sock} $@
}

function setup_wallpapaer ()
{
    local arg=$1

    if [ $# -eq 0 ]; then
        arg="internal"
    fi

    case "$arg" in
        internal)
            cmd=$(cat <<EOF
output "${primary_dev}" background  ${image_dir}/wallpaper-0.png fill
EOF
               )
            exec_swaymsg $cmd
            ;;
        external)
            cmd=$(cat <<EOF
output "${secondary_dev}" background  ${image_dir}/wallpaper-1.png fill
EOF
               )

            exec_swaymsg $cmd
            ;;
        both)
            cmd=$(cat <<EOF
output "${primary_dev}" background  ${image_dir}/wallpaper-0.png fill
EOF
               )
            exec_swaymsg $cmd
            cmd=$(cat <<EOF
output "${secondary_dev}" background  ${image_dir}/wallpaper-1.png fill
EOF
               )
            exec_swaymsg $cmd
            ;;
        *)
            echo "Unknown arg: $arg"
    esac
}

N=`exec_swaymsg -t get_outputs | grep name | wc -l`

if [ $N -eq 1 ]; then
    # setup primary display only.
    echo "Only one monitor detected..."
    exec_swaymsg output ${primary_dev} enable
    exec_swaymsg output ${primary_dev} pos 0 0

    setup_wallpapaer internal
else

    if [ -z "$action" ]; then
        MENU="$(rofi -sep "|" -dmenu -i -p 'DISPLAY' -hide-scrollbar -line-padding 4 \
             -padding 20 -lines 4 <<< \
              " Extend| Mirror| Internal| External")"
    elif [ "$action" = "init" ]; then
        # called during i3 startup, set MENU based on LID status.
        LID_STATUS=/proc/acpi/button/lid/LID/state
        if [ -f ${LID_STATUS} ]; then
            case `/bin/cat ${LID_STATUS} | awk '{print $2}'` in
                closed)
                    MENU=External
                    ;;
                *)
                    # check if internal monitor is disabled.
                    exec_swaymsg -p -t get_outputs  | grep Output | grep eDP |grep -q inactive
                    if [ $? -eq 0 ]; then
                        MENU=External
                    else
                        MENU=Extend
                    fi
                    ;;
            esac
        else
            MENU=Extend
        fi
    else
        MENU="$action"
    fi

    echo "$1 --> MENU: $MENU"

    case "$MENU" in
        *Extend)
            exec_swaymsg output ${primary_dev} enable
            exec_swaymsg output ${secondary_dev} enable

            exec_swaymsg output ${primary_dev} pos 0 0
            exec_swaymsg output ${secondary_dev} pos 1366 0

            setup_wallpapaer both
            ;;
        *Mirror)
            exec_swaymsg output ${primary_dev} enable
            exec_swaymsg output ${secondary_dev} enable

            exec_swaymsg output  ${primary_dev} pos 0 0
            exec_swaymsg output  ${secondary_dev} pos 0 0

            setup_wallpapaer internal
            ;;
        *Internal)
            exec_swaymsg output ${secondary_dev} disable
            exec_swaymsg output ${primary_dev} enable

            setup_wallpapaer internal
            ;;
        *External)
            exec_swaymsg output ${secondary_dev} enable
            exec_swaymsg output ${primary_dev} disable
            setup_wallpapaer external
            ;;
        *)
            ;;
    esac
fi

exec_swaymsg -t get_outputs
