
if  [ -f "$(dirname ${BASH_SOURCE[0]})/profile-$(uname).sh" ] ; then
  source "$(dirname ${BASH_SOURCE[0]})/profile-$(uname).sh"
fi
