
if  [ -f "$(dirname ${BASH_SOURCE[0]})/profile-$(uname).sh" ] ; then
  source "$(dirname ${BASH_SOURCE[0]})/profile-$(uname).sh"
fi

#------------------------------------------------------------------------------
# GGP CLI (Unix)
#------------------------------------------------------------------------------
export GGP_SDK_PATH=${HOME}/YetiSDK/1.48
export PATH=${PATH}:${GGP_SDK_PATH}/dev/bin
