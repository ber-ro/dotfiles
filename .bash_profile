# To the extent possible under law, the author(s) have dedicated all 
# copyright and related and neighboring rights to this software to the 
# public domain worldwide. This software is distributed without any warranty. 
# You should have received a copy of the CC0 Public Domain Dedication along 
# with this software. 
# If not, see <http://creativecommons.org/publicdomain/zero/1.0/>. 

# base-files version 4.2-4

# ~/.bash_profile: executed by bash(1) for login shells.
#echo .bash_profile

# source the users bashrc if it exists
if [ -f "${HOME}/.bashrc" ] ; then
  source "${HOME}/.bashrc"
fi

# Set PATH so it includes user's private bin if it exists
if [ -d "${HOME}/bin" ] ; then
  PATH="${HOME}/bin:${PATH}"
fi

# emacsdir=/c/Users/bernh/sw/emacs-26.2-x86_64/bin
# test -e $emacsdir || emacsdir=/cygdrive/$emacsdir

if [ -n "$MSYSTEM" ]; then
  export HISTFILE=/home/$USER/.bash_history_msys
else
  export EDITOR=emacsclient-w32.exe
fi

if [[ ${TERM} != 'screen' ]]; then
: #  $emacsdir/runemacs.exe &
fi
#[[ ${TERM} != 'screen' && -e /bin/tmux ]] && exec tmux
