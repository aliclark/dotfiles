
###############################################################################

# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

###############################################################################

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

###############################################################################

# Source ANT specific definitions
if [ -f ~/.bashrc-ant ]; then
    . ~/.bashrc-ant
fi

###############################################################################

export EDITOR='emacs'

###############################################################################
# History control

# don't put duplicate lines in the history. See bash(1) for more options
# don't overwrite GNU Midnight Commander's setting of `ignorespace'.
export HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups
# ... or force ignoredups and ignorespace
export HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
export HISTFILESIZE=1000000000
export HISTSIZE=1000000

###############################################################################
# Coloring output

# Coloring for the `less` program

export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

# Coloring for the `ls` program

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# enable color support of ls and also add handy aliases
[ -x /usr/bin/dircolors ] && eval "`dircolors -b`"

export LS_OPTIONS='--color=auto'
alias ls='ls $LS_OPTIONS'
alias dir='dir --color=auto'
alias vdir='vdir --color=auto'

# Coloring for the `grep` program

export GREP_COLOR="1;33"
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

###############################################################################
# Color prompt

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
    PS1='${debian_chroot:+($debian_chroot)}\e[40m'
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
        # We have color support; assume it's compliant with Ecma-48
        # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
        # a case would tend to support setf rather than setaf.)
        color_prompt=yes
    else
        color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    red='\e[0;31m'
    green='\e[0;32m'
    host_font='\e[1;33m'
    pwd_font='\e[1;36m'
    command_font=$green

    # return value visualisation
    PROMPT_COMMAND='ret=$?;'

    return_value='$(if [[ $ret = 0 ]]; then echo -ne "\[$green\] "; else echo -ne "\[$red\]$ret"; fi;)'
    PS1="$return_value \[$host_font\]\h \[$pwd_font\]\w\[$command_font\] "
else
    PS1=' \h \w '
fi

# If this is an xterm set the title to user@host:dir
case "$TERM" in
    xterm*|rxvt*)
        PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
        ;;
    *)
        ;;
esac
unset debian_chroot color_prompt force_color_prompt

###############################################################################
# Misc settings

set bell-style none

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

###############################################################################
# Small utility command

extract () {
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2)   tar xvjf $1    ;;
            *.tar.gz)    tar xvzf $1    ;;
            *.bz2)       bunzip2 $1     ;;
            *.rar)       unrar x $1     ;;
            *.gz)        gunzip $1      ;;
            *.tar)       tar xvf $1     ;;
            *.tbz2)      tar xvjf $1    ;;
            *.tgz)       tar xvzf $1    ;;
            *.zip)       unzip $1       ;;
            *.Z)         uncompress $1  ;;
            *.7z)        7z x $1        ;;
            *)           echo "don't know how to extract '$1'..." ;;
        esac
    else
        echo "'$1' is not a valid file!"
    fi
}

###############################################################################
# Alias definitions.

# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

###############################################################################

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

###############################################################################

if [ -n "$DISPLAY" ]; then
  if [ -z "$INITIAL_XTERM_LVL" ]; then
    INITIAL_XTERM_LVL="$SHLVL"
    if [ $SHLVL -eq "$INITIAL_XTERM_LVL" ]; then
      if [ -z "$STARTED_XTERM_SCREEN" ]; then
        export STARTED_XTERM_SCREEN=1
        screen
      fi
    fi
  fi
else

if [ -z "1" ]; then # temporarily disable screen in vterm

  if [ $SHLVL -eq "1" ]; then
    if [ -z "$STARTED_LOGIN_SCREEN" ]; then
      export STARTED_LOGIN_SCREEN=1
      screen
    fi
  fi

fi

fi
