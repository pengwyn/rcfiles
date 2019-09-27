#!/usr/bin/python

import json, subprocess

output = subprocess.check_output(['i3-msg', '-t', 'get_workspaces'])
workspaces = json.loads(output)

next_num = next(i for i in range(1, 100) if not [ws for ws in workspaces if ws['num'] == i])

import sys
if len(sys.argv) == 2:
    subprocess.call(['i3-msg', 'move container to workspace number %i' % next_num])
subprocess.call(['i3-msg', 'workspace number %i' % next_num])
