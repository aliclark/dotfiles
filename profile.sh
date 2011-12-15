# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

###############################################################################

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

###############################################################################

export SHELL=zsh
export EDITOR=emacs
export BROWSER=firefox

###############################################################################

if [ -d "/usr/course/agm/gPy" ]; then
    PYTHONPATH="/usr/course/agm/gPy:$PYTHONPATH"
fi

export PYTHONPATH

###############################################################################

PATH="$PATH:/usr/sbin:/sbin"

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/local/bin" ]; then
    PATH="$HOME/local/bin:$PATH"
fi

if [ -d "$HOME/bin" ]; then
    PATH="$HOME/bin:$PATH"
fi

if [ -d "$HOME/projects/localscripts" ]; then
    PATH="$HOME/projects/localscripts:$PATH"
fi

if [ -d "$HOME/projects/utalities/bin" ]; then
    PATH="$HOME/projects/utalities/bin:$PATH"
fi

if [ -d "$HOME/build/android-sdk-linux_x86" ]; then
    PATH="$HOME/build/android-sdk-linux_x86/tools:$HOME/build/android-sdk-linux_x86/platform-tools:$PATH"
fi

export PATH

###############################################################################

#if [ -f "$HOME/projects/alibc/bin/libalibc.so.1" ]; then
#    export LD_PRELOAD="$HOME/projects/alibc/bin/libalibc.so.1"
#fi

###############################################################################

# if running bash
#if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
#    if [ -e "$HOME/.bashrc" ]; then
#        . "$HOME/.bashrc"
#    fi
#fi

###############################################################################
