#!/bin/zsh


echo "Start" > /tmp/debugging
function waitforwindow() {
    # echo "Started waiting for $@ at $date" >> /tmp/debugging
    start_time=$(date +%s)
    while true ; do
        sleep 0.1
        if (( $(date +%s) - $start_time > 12 )) ; then
            # Abort after 12 sec
            break
        fi

        # "root" doesn't seem to work here.
        # xwininfo -tree -root | grep $1
        for id in $(xwininfo -tree -name "i3" | grep $@ | awk '{print $1}') ; do
            xwininfo -id $id -shape | grep "IsViewable" > /dev/null
            if [[ $? == 0 ]] ; then
                # echo "Finished waiting at $date" >> /tmp/debugging
                return
            fi
        done
    done
}

# This is in a separate file to make it easier to put sleeps in etc...
# pkill i3icons2 ; i3icons2

dropbox &!

i3-msg 'workspace number 4; move workspace to output primary'
# skypeforlinux &!
# sleep 1
i3-msg 'exec skypeforlinux'

i3-msg 'workspace number 9; move workspace to output primary ; move workspace to output right'
# evolution &!
# sleep 1
i3-msg 'exec evolution'

i3-msg 'workspace number 10; move workspace to output primary'
# chromium &!
i3-msg 'exec chromium'
waitforwindow Chromium
i3-msg 'move workspace to output right'

if [[ $(hostname) != "penganuix" ]] ; then
    i3-msg 'workspace number 8; move workspace to output primary'
    i3-msg 'exec steam-native'
    waitforwindow steam
fi

# If there's no steam at all - break. Otherwise wait until the renderer starts up.
# sleep 2
# while true ; do
#     sleep 0.1
#     pgrep --full 'steamwebhelper' > /dev/null 2>&1
#     if [[ $? != 0 ]] ; then
#         echo "breakign because of steamwebhelper" > /tmp/debugging
#         break
#     fi

#     pgrep --full 'steamwebhelper.*renderer' > /dev/null 2>&1
#     if [[ $? == 0 ]] ; then
#         echo "breakign because of steamwebhelper and renderer" > /tmp/debugging
#         break
#     fi
# done
# sleep 1
# i3-msg 'exec steam-native'


# Leave a blank workspace and chrome open
i3-msg 'workspace number 1; move workspace to output primary'
i3-msg 'workspace number 10'
