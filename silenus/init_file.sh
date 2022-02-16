if [ -f ~/.bashrc ] ; then
  . ~/.bashrc
fi

cd /workspace/src/silenus

# Use docker/install_emacs.sh to install this.
# FIXME(olegat) USER=dockerbuilder causes emacs to not load ~/.emacs
# FIXME(olegat) Using the same .emacs.d dir as google3 emacs causes clashes.
#   Setting HOME telling emacs to use a different directory. ~/.vimrc causes
#   similar problems.
alias emacs="HOME=/workspace/local USER= /workspace/local/bin/emacs"
alias emacsclient="HOME=/workspace/local USER= /workspace/local/bin/emacsclient"
alias vim="HOME=/workspace/local /usr/bin/vim"

# Welcome message:
echo -e '\n  Welcome to the Silenus Docker shell.\n'
cat /workspace/src/silenus/docker/wineglass.utf8.ansi
echo ''
