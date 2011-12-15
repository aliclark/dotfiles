# The following lines were added by compinstall

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' format 'completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' ignore-parents parent pwd
zstyle ':completion:*' insert-unambiguous true
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-prompt '%Sat %p: hit TAB for more, or the character to insert%s'
zstyle ':completion:*' menu select=1
zstyle ':completion:*' original false
zstyle ':completion:*' preserve-prefix '//[^/]##/'
zstyle ':completion:*' select-prompt '%Sscrolling active: current selection at %p%s'
zstyle ':completion:*' verbose true
zstyle :compinstall filename '/home/ali/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd extendedglob notify
bindkey -e
# End of lines configured by zsh-newuser-install

autoload -U promptinit
promptinit
 
setopt completealiases

autoload -U colors && colors

eval `dircolors -b`

setopt autopushd pushdminus pushdsilent pushdtohome
setopt autocd
setopt interactivecomments
setopt nobanghist
setopt noclobber
setopt HIST_REDUCE_BLANKS
setopt HIST_IGNORE_SPACE
setopt HIST_IGNORE_DUPS
setopt SH_WORD_SPLIT
setopt nohup
setopt correct

export PS1="%? $(print '%{\e[1;32m%}%*%{\e[0m%} %{\e[1;33m%}%m%{\e[0m%} %{\e[1;36m%}%~%{\e[0m%} ')"
export PS2="$(print '%{\e[1;32m%}>%{\e[0m%} ')"

backward-delete-to-slash () {
  local WORDCHARS=${WORDCHARS//\//}
  zle .backward-delete-word
}
zle -N backward-delete-to-slash

# Set up auto extension stuff
alias -s html=$BROWSER
alias -s org=$BROWSER
alias -s com=$BROWSER
alias -s net=$BROWSER
alias -s png=feh
alias -s jpg=feh
alias -s sxw=libreoffice
alias -s doc=libreoffice
alias -s zip='unzip'
alias -s gz='tar -xzvf'
alias -s bz2='tar -xjvf'
alias -s java=$EDITOR
alias -s txt=$EDITOR
alias -s c=$EDITOR
alias -s js=$EDITOR
alias -s scm=$EDITOR
alias -s hs=$EDITOR

# Normal aliases
alias ls='ls --color=auto'
alias ll='ls -l'
alias la='ls -A'
alias lh='ll -h'
alias lt='ll -t'
alias lS='lh -S'

