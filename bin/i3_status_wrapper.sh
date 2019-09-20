#!/bin/zsh

# GREEN="\"#00FF00\""
# RED="\"#FF0000\""
# BLUE="\"#0000FF\""
GREEN="'#00FF00'"
RED="'#FF0000'"
BLUE="'#0000FF'"

text=""
rpi_status() {
    val=$(< ~/alarmpi_restart)
    if [[ $val == "YES" ]] ; then
        text="<span foreground=$GREEN>PwrSave</span>"
    else
        text="<span foreground=$RED>StayOn</span>"
    fi
    
    if $(ping -w 1 -c 1 10.1.1.101 >/dev/null 2>&1)
    then
        text="<span foreground=$GREEN>Up</span> $text"
    else
        if [[ $val = "YES" ]] ; then
            text="Down $text"
        else
            text="<span foreground=$RED>Down</span> $text"
        fi
    fi

    lastupdate=$(date --date="$(< /srv/http/rpi/lastupdate.txt )" +"%H:%M %d/%m")
    text="Π ($lastupdate) $text"

    return 0
}

do_line() {
    echo -n "{\"full_text\":\"$@\",
                    \"markup\":\"pango\"},"
}
    

i3status | (
    for i in $(seq 3)
    do
        read line && echo "$line"
    done
             
    while :
    do
        read line
        echo ",["

        if [[ $(hostname) = "pengix" ]] ; then
            rpi_status || exit 1
            do_line $text
        fi

        echo "${line#,\[}"
    done)
