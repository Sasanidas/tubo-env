#!/bin/bash
#

set $*

group=${1%%/*}
action=${1#*/}
device=$2
id=$3
value=$4

scriptdir=${0%/*}

function get_number_of_monitors ()
{
    local number=0
    for DEVICE in /sys/class/drm/card*-*; do
        [ -e "${DEVICE}/status" ] && grep -q "^connected$" "${DEVICE}/status" || continue
        number=$((number+1))
    done
    echo $number
    return $number
}


do_log_unhandled() {
    echo "ACPI event not handled: $@"
}


case "$group" in
    battery)
    ;;
	button)
		case "$action" in
			power)
				/etc/acpi/actions/powerbtn.sh
				;;
            lid)
                case $device in
                    LID)
                        case $id in
                            close)
                                N=`DISPLAY=:0 su -c - yyc xrandr | grep -sw 'connected' | wc -l`
                                if [ $N -gt 1 ]; then
                                    # External monitor connected, change to
                                    # external display...
                                    DISPLAY=:0 su -c - yyc \
                                           "${HOME}/.config/i3/scripts/setup-display.sh External"
                                else
                                    DISPLAY=:0 su -c - yyc "${HOME}/.config/i3/scripts/lock-screen.sh"
                                    sleep 3
                                    systemctl suspend
                                fi
                                ;;
                            *)
                                ;;
                        esac
                        ;;
                    *)
                        logger "ACPI: device ${device} not handled."
                        ;;
                esac
                ;;

			# if your laptop doesnt turn on/off the display via hardware
			# switch and instead just generates an acpi event, you can force
			# X to turn off the display via dpms.  note you will have to run
			# 'xhost +local:0' so root can access the X DISPLAY.
			#lid)
			#	xset dpms force off
			#	;;

			*)	do_log_unhandled $* ;;
		esac
		;;

	ac_adapter)
		case "$value" in
			# Add code here to handle when the system is unplugged
			# (maybe change cpu scaling to powersave mode).  For
			# multicore systems, make sure you set powersave mode
			# for each core!
			#*0)
			#	cpufreq-set -g powersave
			#	;;

			# Add code here to handle when the system is plugged in
			# (maybe change cpu scaling to performance mode).  For
			# multicore systems, make sure you set performance mode
			# for each core!
			#*1)
			#	cpufreq-set -g performance
			#	;;

			*)	do_log_unhandled $* ;;
		esac
		;;
	*)	do_log_unhandled $* ;;
esac
