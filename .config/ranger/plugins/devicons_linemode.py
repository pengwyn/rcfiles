import ranger.api
from ranger.core.linemode import LinemodeBase, SizeMtimeLinemode, PermissionsLinemode
from ranger.core.linemode import LinemodeBase, SizeMtimeLinemode
from .devicons import *

@ranger.api.register_linemode
class DevIconsLinemode(LinemodeBase):
  name = "devicons"

  uses_metadata = False

  def filetitle(self, file, metadata):
    return devicon(file) + ' ' + file.relative_path

@ranger.api.register_linemode
class DevIconsLinemodeFile(LinemodeBase):
  name = "filename"

  def filetitle(self, file, metadata):
    return devicon(file) + ' ' + file.relative_path
  

@ranger.api.register_linemode
class DannyTime(LinemodeBase):
  name = "danny_time"

  def filetitle(self, file, metadata):
    temp = devicon(file) + ' ' + file.relative_path
    return temp
  
  def infostring(self, file, metadata):
    temp = SizeMtimeLinemode.infostring(self, file, metadata)
    return temp

@ranger.api.register_linemode
class DannyPermissions(LinemodeBase):
  name = "danny_perm"

  def filetitle(self, file, metadata):
    temp = devicon(file) + ' ' + file.relative_path
    return temp
  
  def infostring(self, file, metadata):
    temp = "%s %s %s" % (file.user, file.group, file.get_permission_string())
    return temp
