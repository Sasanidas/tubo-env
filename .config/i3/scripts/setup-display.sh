#!/bin/bash
#
# Author: Yang,Ying-chao <yingchao.yang@icloud.com>, 2019-08-24
#

source ${HOME}/.config/i3/scripts/common.sh

action=$1
N=`xrandr | grep -sw 'connected' | wc -l`

image_dir=${HOME}/.config/wallpapers

function pick-wallpaper ()
{
    N=1
    if [ $# -ne 0 ]; then
        N=$1
    fi

    files=`ls ${image_dir}/*{jpg,JPG} | sort -R | tail -$N`
    echo $files > /tmp/CURRENT_WALLPAPERS
    echo $files
}

function setup_wallpapaer ()
{
    local arg=$1

    if [ $# -eq 0 ]; then
        arg="internal"
    fi

    case "$arg" in
        internal)
            feh --bg-scale `pick-wallpaper`
            ;;
        external)
            feh --bg-scale `pick-wallpaper`
            ;;
        both)
            feh --bg-scale `pick-wallpaper 2`
            ;;
        *)
            echo "Unknown arg: $arg"
    esac

    sleep 2 && ${HOME}/.config/i3/scripts/lock-screen.sh prepare &
}

function launch-polybar ()
{
    local cur_mode="$@"
    if [ -z "$cur_mode" ]; then
        return
    fi

    local old_mode=
    if [ -f /tmp/CURRENT_DISPLAY_MODE ]; then
        old_mode=`cat /tmp/CURRENT_DISPLAY_MODE`
    fi

    echo $cur_mode > /tmp/CURRENT_DISPLAY_MODE

    local relauch=

    ps aux | grep -q '[p]olybar'
    if [ $? -ne 0 ]; then
        relauch=1
    elif [ "${cur_mode}" != "${old_mode}" ]; then
        echo "CUR: ${cur_mode}, OLD: ${old_mode}"
        relauch=1
    fi

    if [ $relauch ]; then
        ~/.config/polybar/launch.sh
    fi
}

if [ $N -eq 1 ]; then
    # setup primary display only.
    echo "Only one monitor detected..."
    setup_wallpapaer internal
    launch-polybar internal
else
    if [ -z "$action" ]; then
        MENU="$(rofi -sep "|" -dmenu -i -p 'DISPLAY' -hide-scrollbar -line-padding 4 \
             -padding 20 -lines 4 <<< \
              " Extend| Mirror| Internal| External")"
        MENU=$(echo $MENU | awk '{print $2}')

    elif [ "$action" = "init" ]; then
        # called during i3 startup, set MENU based on LID status.
        LID_STATUS=/proc/acpi/button/lid/LID/state
        if [ -f ${LID_STATUS} ]; then
            case `/bin/cat ${LID_STATUS} | awk '{print $2}'` in
                closed)
                    MENU=External
                    ;;
                *)
                    MENU=Extend
                    ;;
            esac
        else
            MENU=Extend
        fi
    else
        MENU="$action"
    fi

    echo "MENU: $MENU"

    case "$MENU" in
        *Extend)
            xrandr --output ${INTERNAL_DEV} --auto \
                   --output ${EXTERNAL_DEV} --auto --right-of ${INTERNAL_DEV}
            setup_wallpapaer both
            ;;
        *Mirror)
            xrandr --output ${INTERNAL_DEV} --auto \
                   --output ${EXTERNAL_DEV} --auto --same-as ${INTERNAL_DEV}
            setup_wallpapaer internal
            ;;
        *Internal)
            xrandr --output ${INTERNAL_DEV} --auto --output ${EXTERNAL_DEV} --off
            setup_wallpapaer internal
            ;;
        *External)
            xrandr --output ${EXTERNAL_DEV} --auto --output ${INTERNAL_DEV} --off
            setup_wallpapaer external
            ;;
        *)
            ;;
    esac

    launch-polybar $MENU
fi
