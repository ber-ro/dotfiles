# To the extent possible under law, the author(s) have dedicated all 
# copyright and related and neighboring rights to this software to the 
# public domain worldwide. This software is distributed without any warranty. 
# You should have received a copy of the CC0 Public Domain Dedication along 
# with this software. 
# If not, see <http://creativecommons.org/publicdomain/zero/1.0/>. 

# base-files version 4.2-4

# ~/.bashrc: executed by bash(1) for interactive shells.

# If not running interactively, don't do anything
[[ "$-" != *i* ]] && return

# Shell Options
# Don't wait for job termination notification
set -o notify

# Use case-insensitive filename globbing
shopt -s nocaseglob

# History Options
export HISTCONTROL=ignoredups:erasedups
shopt -s histappend
PROMPT_COMMAND="history -n; history -w; history -c; history -r; $PROMPT_COMMAND"
HISTSIZE=1000
HISTFILESIZE=2000

TMP=/cygdrive/c/Users/rotterb/AppData/Local/Temp
TEMP=$TMP
#export NODE_PATH=D:\\Benutzer-Profile\\rotterb\\AppData\\Roaming\\npm\\node_modules
PERLLIB=$PERLLIB:~/bin
LANG=C # ediff
export DICPATH=/cygdrive/c/Programme/office/libreoffice/share/extensions/dict-de
export DICTIONARY=de_DE_frami,en_US,en_geo,en_GB,en_med
export DISPLAY=:0.0

if [[ $INSIDE_EMACS ]]; then
  export PS1="\\w\\$ "
  export PAGER="/bin/cat"
else
  export PAGER="/bin/less"
fi

# Aliases
#
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

alias df='df -h'
alias du='du -h'

alias ls='ls -hF --color=tty'
alias ll='ls -l'
alias la='ls -A'
alias l='ls -CF'

alias h=history
alias dgit='git --git-dir ~/.dotfiles/.git --work-tree=$HOME'
