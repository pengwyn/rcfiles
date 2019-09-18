#!/bin/zsh

term=xfce4-terminal

pid=$(xprop -id $(xdotool getwindowfocus) | awk '/PID/ { print $3 }')

echo $(date) here $pid $term $? > ~/testlog

if [[ "$pid" == <-> ]] ; then
    cd /proc/$pid/cwd
fi

exec $term "$@"
