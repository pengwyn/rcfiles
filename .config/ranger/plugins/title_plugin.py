import ranger.api
import os
import sys

old_hook_init = ranger.api.hook_init

def hook_init(fm):
    def on_cd():
        fm.notify("On cd")
        if fm.thisdir:
            fm.notify("In thisdir")
            # title = os.path.basename(fm.thisdir.path)
            title = fm.thisdir.path
            sys.stdout.write("\033]0;ranger pwd: "+title+"\007")
            sys.stdout.flush()

    fm.notify("In here")
    fm.signal_bind('cd', on_cd)
    return old_hook_init(fm)

ranger.api.hook_init = hook_init
