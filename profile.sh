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

PATH="$PATH"

if [ -d "/usr/sbin" ]; then
    PATH="$PATH:/usr/sbin"
fi

if [ -d "/sbin" ]; then
    PATH="$PATH:/sbin"
fi

if [ -d "$HOME/build/android-sdk-linux_x86" ]; then
    PATH="$PATH:$HOME/build/android-sdk-linux_x86/tools:$HOME/build/android-sdk-linux_x86/platform-tools"
fi

if [ -d "$HOME/bin" ]; then
    PATH="$HOME/bin:$PATH"
fi

export PATH

###############################################################################

#if [ -f "$HOME/projects/alibc/bin/libalibc.so.1" ]; then
#    export LD_PRELOAD="$HOME/projects/alibc/bin/libalibc.so.1"
#fi

###############################################################################

