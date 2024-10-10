# bash profile by Mario Gajardo Tassara
# updated at 10-10-2024 20:05:00

eval "$(/opt/homebrew/bin/brew shellenv)"

# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
  *i*) ;;
  *) return ;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
  debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
  xterm-color | *-256color) color_prompt=yes ;;
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
  PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
  PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
  xterm* | rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
  *) ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
  test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
  alias ls='ls --color=auto'
  #alias dir='dir --color=auto'
  #alias vdir='vdir --color=auto'

  alias grep='grep --color=auto'
  alias fgrep='fgrep --color=auto'
  alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
  . ~/.bash_aliases
fi

# brew bash completition
if type brew &>/dev/null; then
  HOMEBREW_PREFIX="$(brew --prefix)"
  if [[ -r "${HOMEBREW_PREFIX}/etc/profile.d/bash_completion.sh" ]]; then
    source "${HOMEBREW_PREFIX}/etc/profile.d/bash_completion.sh"
  else
    for COMPLETION in "${HOMEBREW_PREFIX}/etc/bash_completion.d/"*; do
      [[ -r "${COMPLETION}" ]] && source "${COMPLETION}"
    done
  fi
fi

# this is for you apple, fuuuckkk you!
export BASH_SILENCE_DEPRECATION_WARNING=1

# Load the shell dotfiles, and then some:
# * ~/.path can be used to extend `$PATH`.
# * ~/.extra can be used for other settings you don’t want to commit.

for file in ~/.{path,bash_prompt,exports,aliases,functions,extra}; do
  [ -r "$file" ] && source "$file"
done
unset file

# lang
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
export LC_ALL=C

# nvim as vim 
alias vim='nvim'

# better grep
alias grep='ggrep --color=auto'

# bash reload
alias bashrl='source ~/.bash_profile'

# brew updates
alias apug='brew update && brew upgrade && brew autoremove && brew cleanup'

# nmap scan all
alias nmaps='sudo nmap -sn 192.168.1.1/24'

# macchanger
alias maclist='macchanger -s en0'
alias macrand='macchanger -r en0'
alias macdef='macchanger -p en0'

#tor
alias tors='sudo networksetup -setsocksfirewallproxystate Wi-Fi on'
alias tore='sudo networksetup -setsocksfirewallproxystate Wi-Fi off'

# ftp
alias ftp='ncftp'

# external ip
alias eip='curl icanhazip.com'

# localip
HOST_IP=$(ifconfig | grep -oP '(\s)\d+(\.\d+.\d+.\d+)' | sed -n '2p')
alias lip='echo $HOST_IP'
# all local ips
HOST_ALL_IP=$(ifconfig | grep -oP '(\s)\d+(\.\d+.\d+.\d+)')
alias aip='echo $HOST_ALL_IP'

# top -> btop
alias top='btop'

# cat -> bat
alias cat='bat --theme ansi'
#man reader -> bat
export MANPAGER="sh -c 'col -bx | bat --theme ansi -l man -p'"

export EDITOR=nvim
export CLICOLOR=1

# cd up
alias cd.='cd ..'
alias cd..='cd ../..'
alias cd...='cd ../../..'

# silence bell
export LESS="$LESS -R -Q"

# sort folder by size
alias dsort='du -m --max-depth 1 | sort -rn'

# list by size
alias dus='du -shc *'

# bash colors
colorflag="--color"

#export LS_COLORS='no=00:fi=00:di=00;32:ln=01;31:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:ex=01;31:*.tar=00;31:*.tgz=00;31:*.arj=00;31:*.taz=00;31:*.lzh=00;31:*.zip=00;31:*.z=00;31:*.Z=00;31:*.gz=00;31:*.bz2=00;31:*.deb=00;31:*.rpm=00;31:*.jar=00;31:*.jpg=00;35:*.jpeg=00;35:*.gif=00;35:*.bmp=00;35:*.pbm=00;35:*.pgm=00;35:*.ppm=00;35:*.tga=00;35:*.xbm=00;35:*.xpm=00;35:*.tif=00;35:*.tiff=00;35:*.png=00;35:*.mov=00;35:*.mpg=00;35:*.mpeg=00;35:*.avi=00;35:*.fli=00;35:*.gl=00;35:*.dl=00;35:*.xcf=00;35:*.xwd=00;35:*.ogg=00;35:*.mp3=00;35:*.wav=0;35:*.c=00;32:*.h=00;32:*.cpp=00;36:*.rs=00;36:*.m=00;33:*.xib=00;31:*.swp=02;37:*.docx=00;31:*.doc=00;31:*.xlsx=00;31:*.xls=00;31:*.txt=00;31:*.log=00;33:'

# recursive ls
alias lr='ls -R $PWD/*'

# list all files colorized in long format
alias ll='ls -laHhFl'

# list all files colorized in long format, including dot files
alias la="ls -lha"

# List only directories
alias lsd='ls -l | grep "^d"'

# clear screen
alias c='clear'

# process
alias ps='ps -ax | more'
# copy
alias cp='cp -iv' # Preferred 'cp' implementation
# move
alias mv='mv -iv' # Preferred 'mv' implementation
# make dir
alias mkdir='mkdir -pv' # Preferred 'mkdir' implementation
# list
alias less='less -FSRXc' # Preferred 'less' implementation

# comando para comprimir en zip
zipf() { zip -r "$1".zip "$1"; }

# find pid
alias pidof='findPid'
findPid() { lsof -t -c "$@"; }

# ---------------------------
#   6.  NETWORKING
# ---------------------------
alias netCons='lsof -i'                           # netCons:      Show all open TCP/IP sockets
alias flushDNS='dscacheutil -flushcache'          # flushDNS:     Flush out the DNS Cache
alias lsock='sudo /usr/sbin/lsof -i -P'           # lsock:        Display open sockets
alias lsockU='sudo /usr/sbin/lsof -nP | grep UDP' # lsockU:       Display only open UDP sockets
alias lsockT='sudo /usr/sbin/lsof -nP | grep TCP' # lsockT:       Display only open TCP sockets
alias ipInfo0='ipconfig getpacket en0'            # ipInfo0:      Get info on connections for en0
alias ipInfo1='ipconfig getpacket en1'            # ipInfo1:      Get info on connections for en1
alias openPorts='sudo lsof -i | grep LISTEN'      # openPorts:    All listening connections
alias showBlocked='sudo ipfw list'                # showBlocked:  All ipfw rules inc/ blocked IPs

# simple note pad
npad() {
  rememberfile="/Users/mario/OneDrive/Documentos/remember.txt"
  if [ $# -eq 0 ]; then
    echo "" >>$rememberfile
    echo "\nIngresa la nota, para finalizar pulsa ^C: "
    date=$(date +"(%d-%m-%Y -- %H:%M:%S)")
    echo "$date" >>$rememberfile
    echo "" >>$rememberfile
    cat - >>$rememberfile
  else
    echo "" >>$rememberfile
    date=$(date +"(%d-%m-%Y -- %H:%M:%S)")
    echo "$date" >>$rememberfile
    echo "" >>$rememberfile
    echo "$@" >>$rememberfile
  fi
}

# net status
rr() {
  echo -e "\n Open Sockets: $NC "
  lsock | less
  echo -e "\n Open Ports: $NC "
  openPorts | less
  echo -e "\n Bloqued by IPFW: $NC "
  showBlocked
}

# ii:  display useful host related informaton
# -------------------------------------------------------------------
ii() {
  echo -e "\nYou are logged on ${RED}$HOST"
  echo -e "\nAdditionnal information:$NC "
  uname -a
  echo -e "\n${RED}Users logged on:$NC "
  w -h
  echo -e "\n${RED}Current date :$NC "
  date
  echo -e "\n${RED}Machine stats :$NC "
  uptime
  echo
}

# extract multiple compression formats
extract() {
  if [ -f $1 ]; then
    case $1 in
      *.tar.bz2) tar xjf $1 ;;
      *.tar.gz) tar xzf $1 ;;
      *.bz2) bunzip2 $1 ;;
      *.rar) unrar e $1 ;;
      *.gz) gunzip $1 ;;
      *.tar) tar xf $1 ;;
      *.tbz2) tar xjf $1 ;;
      *.tgz) tar xzf $1 ;;
      *.zip) unzip $1 ;;
      *.Z) uncompress $1 ;;
      *.7z) 7z x $1 ;;
      *) echo "'$1' no se puede extraer via extract()" ;;
    esac
  else
    echo"'$1' no es un fichero valido"
  fi
}

# GIT
# git status
alias gitus='git status'
# git commit -> push
alias gitpush='sh gitpush.sh updates'

# alias dev dirs
alias cdscript='cd $HOME/github/scripts'
alias cdgit='cd $HOME/github'
alias cdvim='cd $HOME/.config/nvim'

# alias cargo Rust
alias ccc='cargo clean'
alias ccb='cargo build'
alias ccr='cargo run'
alias cct='cargo test'
alias rustag='rusty-tags emacs'

# alias zig builds
alias zigsmall='zig build -Drelease-small=true'
alias zigfast='zig build -Drelease-fast=true'
alias zigclean='zig build uninstall'
alias zigrun='zig build run -- ~/Desktop/resource.ara'
alias zigtest='zig test src/main.zig'
alias zigbuild='zig build -Doptimize=ReleaseSafe'

# go stuff
alias igopls='cd $HOME/go && sudo go install golang.org/x/tools/gopls@latest'

# paths
export PATH="$HOME/github/scripts:$PATH"
export PATH="$HOME/github/zig_lazy_updater:$PATH"
export PATH="$HOME/zig:$PATH"
export PATH="$HOME/zls/zig-out/bin:$PATH"
export PATH="$HOME/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/go/bin:$PATH"

export PATH="/opt/homebrew/opt/llvm/bin:$PATH"
export PATH="/opt/homebrew/opt/gopls/bin:$PATH"
export PATH="/opt/homebrew/opt/grep/libexec/gnubin:$PATH"
export PATH="/opt/homebrew/opt/ruby/bin:$PATH"
export PATH="/opt/homebrew/lib/ruby/gems/3.3.0/bin:$PATH"

# cpaths
export CPATH=$CPATH:/opt/homebrew/include
export CPATH=$CPATH:/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include
export LDFLAGS="-L/opt/homebrew/opt/llvm/lib"
export CPPFLAGS="-I/opt/homebrew/opt/llvm/include"
export LDFLAGS="-L/opt/homebrew/opt/ruby/lib"
export CPPFLAGS="-I/opt/homebrew/opt/ruby/include"
export PKG_CONFIG_PATH="/opt/homebrew/opt/ruby/lib/pkgconfig"
# go fuckery
export GOPATH=~/go

# fzf
[ -f ~/.fzf.bash ] && source ~/.fzf.bash

# Use ~~ as the trigger sequence instead of the default **
# export FZF_COMPLETION_TRIGGER='~~'

# Set the DISPLAY
export DISPLAY=:0

# rust stuff
