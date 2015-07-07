#!/bin/bash

# small power menu using rofi, i3, systemd and pm-utils
# (last 3 dependencies are adjustable below)
# tostiheld, 2016

MENU="$(rofi -sep "|" -dmenu -i -p 'System' -hide-scrollbar -line-padding 4 \
 -padding 20 -lines 4 <<< \
 " Lock|﫼 Logout|⏾ Sleep|  Reboot| Shutdown")"

case "$MENU" in
    *Lock) ${HOME}/.config/i3/scripts/lock-screen.sh;;
    *Logout) i3-msg exit;;
    *Sleep)  systemctl suspend;;
    *Reboot) systemctl reboot ;;
    *Shutdown) systemctl -i poweroff
esac
