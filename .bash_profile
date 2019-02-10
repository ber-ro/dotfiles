# To the extent possible under law, the author(s) have dedicated all 
# copyright and related and neighboring rights to this software to the 
# public domain worldwide. This software is distributed without any warranty. 
# You should have received a copy of the CC0 Public Domain Dedication along 
# with this software. 
# If not, see <http://creativecommons.org/publicdomain/zero/1.0/>. 

# base-files version 4.2-4

# ~/.bash_profile: executed by bash(1) for login shells.

# source the users bashrc if it exists
if [ -f "${HOME}/.bashrc" ] ; then
  source "${HOME}/.bashrc"
fi

# Set PATH so it includes user's private bin if it exists
# if [ -d "${HOME}/bin" ] ; then
#   PATH="${HOME}/bin:${PATH}"
# fi

export PATH="/usr/local/bin:/usr/bin:/cygdrive/c/Program Files/Common Files/Microsoft Shared/Microsoft Online Services:/cygdrive/c/Program Files (x86)/Common Files/Microsoft Shared/Microsoft Online Services:/cygdrive/c/Windows/system32:/cygdrive/c/Windows:/cygdrive/c/Windows/System32/Wbem:/cygdrive/c/Windows/System32/WindowsPowerShell/v1.0:/cygdrive/c/Program Files (x86)/Microsoft Application Virtualization Client:/cygdrive/c/Windows/System32/WindowsPowerShell/v1.0:/cygdrive/c/Program Files/Citrix/System32:/cygdrive/c/Program Files/Citrix/ICAService:/cygdrive/c/Program Files/Citrix/Virtual Desktop Agent:/cygdrive/c/Windows/System32/WindowsPowerShell/v1.0:/cygdrive/c/Users/$USERNAME/jdk-11.0.1/bin:/cygdrive/c/Users/$USERNAME/apache-maven-3.5.4/bin/:/cygdrive/c/Program Files/nodejs:$HOME/bin"
export JAVA_HOME=/cygdrive/c/Users/$USERNAME/jdk-11.0.1
export EDITOR=emacsclient-w32

if [[ ${TERM} != 'screen' ]]; then
 /usr/bin/emacs & #emacs-w32.exe &
fi
[[ ${TERM} != 'screen' && -e /bin/tmux ]] && exec tmux
