
if  [ -f "$(dirname ${BASH_SOURCE[0]})/profile-$(uname).sh" ] ; then
  source "$(dirname ${BASH_SOURCE[0]})/profile-$(uname).sh"
fi

#------------------------------------------------------------------------------
# Yeti GGP CLI (Unix)
#------------------------------------------------------------------------------
export YETI_SDK_PATH=${HOME}/YetiSDK
export PATH=${PATH}:${HOME}/YetiSDK/dev/bin
