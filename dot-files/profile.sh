
if [[ $(uname -r) =~ rodete ]] ; then
  source "$(dirname ${BASH_SOURCE[0]})/profile-Rodete.sh"
elif  [ -f "$(dirname ${BASH_SOURCE[0]})/profile-$(uname).sh" ] ; then
  source "$(dirname ${BASH_SOURCE[0]})/profile-$(uname).sh"
fi

if  [ -f "${HOME}/.cargo/env" ] ; then
  source "${HOME}/.cargo/env"
fi

export EMACSCLIENT="emacsclient -t -a ''"
alias ec=$EMACSCLIENT
alias github="~/la-casa/scripts/github.bash"
