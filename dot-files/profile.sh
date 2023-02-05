
if [[ $(uname -r) =~ rodete ]] ; then
  source "$(dirname ${BASH_SOURCE[0]})/profile-Rodete.sh"
elif  [ -f "$(dirname ${BASH_SOURCE[0]})/profile-$(uname).sh" ] ; then
  source "$(dirname ${BASH_SOURCE[0]})/profile-$(uname).sh"
fi

if  [ -f "${HOME}/.cargo/env" ] ; then
  source "${HOME}/.cargo/env"
fi

#------------------------------------------------------------------------------
# GGP CLI (Unix)
#------------------------------------------------------------------------------
export GGP_SDK_PATH=${HOME}/YetiSDK
export PATH=${PATH}:${GGP_SDK_PATH}/dev/bin
export SILENUS_TOOLCHAIN=${HOME}/silenus-toolchain/usr
