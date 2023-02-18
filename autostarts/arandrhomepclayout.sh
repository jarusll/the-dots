#!/bin/sh
# Check if xrandr is installed
if ! command -v xrandr &> /dev/null
then
		echo "xrandr could not be found"
		exit
fi
xrandr --output DisplayPort-0 --off --output DisplayPort-1 --off --output DisplayPort-2 --mode 1280x720 --pos 1920x0 --rotate left --output HDMI-A-0 --mode 1920x1080 --pos 0x100 --rotate normal
