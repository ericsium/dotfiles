######################################
# Functions
######################################
path_remove() {
    PATH=$(echo -n $PATH | awk -v RS=: -v ORS=: '$0 != "'$1'"' |sed 's/:$//')
}

path_append() {
    path_remove "$1"
    PATH="${PATH:+"$PATH:"}$1"
}

path_prepend() {
    path_remove "$1"
    PATH="$1${PATH:+":$PATH"}"
}

# Update dotfiles
dfu() {
    (
        cd ~/.dotfiles && git pull --ff-only && ./install -q
    )
}

# Allow local customizations in the ~/.bashrc_local_before file
if [ -f ~/.bashrc_local_before ]; then
    source ~/.bashrc_local_before
fi

######################################
# Settings
######################################
# Required for 256-bit ANSI color)
if [ -n "$INSIDE_EMACS" ]; then
        TERM=xterm-256color
fi

# Only ok for very trusted environment, recommended 027
umask 002

EDITOR=emacs
PAGER=cat
MANPAGER=cat

# https://www.gnu.org/software/bash/manual/html_node/Bash-History-Facilities.html
HISTSIZE=5000
HISTFILESIZE=50000

# http://www.caliban.org/bash/index.shtml
#
# $HISTIGNORE - Set this to to avoid having consecutive duplicate
# commands and other not so useful information appended to the history
# list. This will cut down on hitting the up arrow endlessly to get to
# the command before the one you just entered twenty times. It will
# also avoid filling a large percentage of your history list with
# useless commands.
HISTIGNORE="&:ls:ls *:mutt:[bf]g:exit:bg:fg:history"

# https://linux.101hacks.com/command-line-history/display-timestamp-using-histtimeformat/
# Add timestamp to history entries
HISTTIMEFORMAT='%F %T '

# ignoreeof - Ordinarily, issuing Ctrl-D at the prompt will log you out
# of an interactive shell. This can be annoying if you regularly need
# to type Ctrl-D in other situations, for example, when trying to
# disconnect from a Telnet session. In such a situation, hitting
# Ctrl-D once too often will close your shell, which can be very
# frustrating. This option (set with set -o ignoreeof disables the use
# of Ctrl-D to exit the shell.
set -o ignoreeof

# shopt options
# You can set each of the options below with shopt -s <option>.
#
# For full details see: https://www.gnu.org/software/bash/manual/html_node/The-Shopt-Builtin.html
#
# autocd - If set, a command name that is the name of a directory is
# executed as if it were the argument to the cd command. This option
# is only used by interactive shells.
#
# cdspell - This will correct minor spelling errors in a cd command,
# so that instances of transposed characters, missing characters and
# extra characters are corrected without the need for retyping.
shopt -s cdspell

# cmdhist - This is very much a matter of taste. Defining this will
# Cause multi-line commands to be appended to your bash history as a
# single line command. This makes for easy command editing.
shopt -s cmdhist

# dirspell - If set, Bash attempts spelling correction on directory
# names during word completion if the directory name initially
# supplied does not exist.
shopt -s dirspell

# dotglob - This one allows files beginning with a dot ('.') to be
# returned in the results of path-name expansion.
shopt -s dotglob

# extglob - This will give you ksh-88 egrep-style extended pattern
# matching or, in other words, turbo-charged pattern matching within
# bash. The available operators are:
#
# ?(pattern-list)
#   Matches zero or one occurrence of the given patterns
# *(pattern-list)
#   Matches zero or more occurrences of the given patterns
# +(pattern-list)
#   Matches one or more occurrences of the given patterns
# @(pattern-list)
#   Matches exactly one of the given patterns
# !(pattern-list)
#   Matches anything except one of the given patterns
#
# Here's an example. Say, you wanted to install all RPMs in a given
# directory, except those built for the noarch architecture. You might
# use something like this:
#
# rpm -Uvh /usr/src/RPMS/!(*noarch*)
#
# These expressions can be nested, too, so if you wanted a directory
# listing of all non PDF and PostScript files in the current
# directory, you might do this:
#
# ls -lad !(*.p@(df|s))
#
#shopt -s extglob

# histappend - If set, the history list is appended to the file named
# by the value of the HISTFILE variable when the shell exits, rather
# than overwriting the file.
shopt -s histappend

PS1='\n[\d \@\n\w\n\u@\h]\$ '

######################################
# Bootstrap
######################################
path_prepend "$HOME/.local/bin"
path_prepend "$HOME/.dotfiles/bin"

######################################
# External settings
######################################
# pip should only run if there is a virtualenv currently activated
export PIP_REQUIRE_VIRTUALENV=true

# Cache pip-installed packages to avoid re-downloading
export PIP_DOWNLOAD_CACHE=$HOME/.pip/cache

# Python startup file
export PYTHONSTARTUP=$HOME/.pythonrc

######################################
# Aliases
######################################
# Use colors in coreutils utilities output
alias ls='ls --color=auto'
alias grep='grep --color'

# git related aliases
alias gag='git exec ag'

# find related aliases
alias findf='find . -type f | fgrep'
alias findfx='find . -type f | xargs fgrep'

alias hg='history | grep'

alias rmrf='rm -rf'

######################################
# Plugins
######################################
# dircolors
# for dark backgrounds ansi-light seems the best
# This can work in emacs 'shell' mode but requires two parts
# 1. (see settings.bash - TERM=dumb-emacs-ansi)
# 2. custom termincal to define dumb-emacs-ansi (see .terminfo)
if [[ "$(tput colors)" == "256" ]]; then
    # eval "$(dircolors ~/.shell/plugins/dircolors-solarized/dircolors.ansi-universal)"
    # eval "$(dircolors ~/.shell/plugins/dircolors-solarized/dircolors.256dark)"
    # eval "$(dircolors ~/.shell/plugins/dircolors-solarized/dircolors.ansi-dark)"
    eval "$(dircolors ~/.shell/plugins/dircolors-solarized/dircolors.ansi-light)"
fi

# http://www.caliban.org/bash/index.shtml#completion
# Enable bash completions
source /etc/profile.d/bash_completion.sh

######################################
# Local Customizations Post Hook
######################################
# Allow local customizations in the ~/.bashrc_local_after file
if [ -f ~/.bashrc_local_after ]; then
    source ~/.bashrc_local_after
fi

######################################
# Misc
######################################

# Enable Python
export PATH="/home/ewwhite/.pyenv/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"
