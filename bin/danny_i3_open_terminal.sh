#!/bin/zsh

term=xfce4-terminal

allinfo=$(xprop -id $(xdotool getwindowfocus))

namepwd=$(echo $allinfo | perl -n -e'/^WM_NAME.*=.*pwd: (.*)"/ && print $1')
if [[ $? == 0 ]] ; then
    cd $namepwd
    echo $(date) here2 $namepwd $term $? > ~/testlog
	echo $allinfo >> ~/testlog
else
    pid=$(echo $allinfo | awk '/PID/ { print $3 }')
    if [[ "$pid" == <-> ]] ; then
        cd /proc/$pid/cwd
    fi
    echo $(date) here $pid $term $? > ~/testlog
fi

exec $term "$@"
