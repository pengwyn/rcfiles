#!/bin/zsh

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
sleep 1
i3-msg 'move workspace to output right'

i3-msg 'workspace number 8; move workspace to output primary'
steam-native &!

# If there's no steam at all - break. Otherwise wait until the renderer starts up.
sleep 2
while true ; do
    sleep 0.1
    pgrep --full 'steamwebhelper' > /dev/null 2>&1
    if [[ $? != 0 ]] ; then
        echo "breakign because of steamwebhelper" > /tmp/debugging
        break
    fi

    pgrep --full 'steamwebhelper.*renderer' > /dev/null 2>&1
    if [[ $? == 0 ]] ; then
        echo "breakign because of steamwebhelper and renderer" > /tmp/debugging
        break
    fi
done
sleep 1
# i3-msg 'exec steam-native'


# Leave a blank workspace and chrome open
i3-msg 'workspace number 1; move workspace to output primary'
i3-msg 'workspace number 10'
