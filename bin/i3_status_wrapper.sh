#!/bin/zsh

# GREEN="\"#00FF00\""
# RED="\"#FF0000\""
# BLUE="\"#0000FF\""
GREEN="'#00FF00'"
RED="'#FF0000'"
BLUE="'#0000FF'"

text=""
rpi_status() {
    text=""

    if [[ $(hostname) = "pengix" ]] ; then
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
    else
        data="$(curl pengix/rpi/lastupdate.txt --connect-timeout 1)"
        if [[ $? == 0 ]] ; then
            lastupdate=$(date --date="$data" +"%H:%M %d/%m")
        else
            lastupdate="N/C"
        fi
    fi
    text=" ($lastupdate) $text"

    return 0
}




# Stolen from https://github.com/i3/i3status/blob/master/contrib/net-speed.sh

ifaces=$(ls /sys/class/net | grep -E '^(eth|wlan|enp|wlp)')

last_time=0
last_rx=0
last_tx=0
rate=""

readable() {
  local bytes=$1
  local kib=$(( bytes >> 10 ))
  if [ $kib -lt 0 ]; then
    echo "? K"
  elif [ $kib -gt 1024 ]; then
    local mib_int=$(( kib >> 10 ))
    local mib_dec=$(( kib % 1024 * 976 / 10000 ))
    if [ "$mib_dec" -lt 10 ]; then
      mib_dec="0${mib_dec}"
    fi
    echo "${mib_int}.${mib_dec} M"
  else
    echo "${kib} K"
  fi
}

update_rate() {
  local time=$(date +%s)
  local rx=0 tx=0 tmp_rx tmp_tx

  for iface in $ifaces; do
    read tmp_rx < "/sys/class/net/${iface}/statistics/rx_bytes"
    read tmp_tx < "/sys/class/net/${iface}/statistics/tx_bytes"
    rx=$(( rx + tmp_rx ))
    tx=$(( tx + tmp_tx ))
  done

  local interval=$(( $time - $last_time ))
  if [ $interval -gt 0 ]; then
    rate="$(readable $(( (rx - last_rx) / interval )))↓ $(readable $(( (tx - last_tx) / interval )))↑"
  else
    rate=""
  fi

  last_time=$time
  last_rx=$rx
  last_tx=$tx
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

        rpi_status || exit 1
        do_line $text

        update_rate
        do_line $rate

        echo "${line#,\[}"
    done)
