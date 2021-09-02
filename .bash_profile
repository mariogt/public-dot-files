# Mario Gajardo Tassara
# MarioGT Software
# https://www.mariogt.com
# mario@mariogt.com

# this is for you apple, I don't want zsh!
export BASH_SILENCE_DEPRECATION_WARNING=1

# Load the shell dotfiles, and then some:
# * ~/.path can be used to extend `$PATH`.
# * ~/.extra can be used for other settings you don’t want to commit.

for file in ~/.{path,bash_prompt,exports,aliases,functions,extra}; do
    [ -r "$file" ] && source "$file"
done
unset file

# save all your terminal commands history
export PROMPT_COMMAND='if [ "$(id -u)" -ne 0 ]; then echo "$(date "+%Y-%m-%d.%H:%M:%S") $(pwd) $(history 1)" >> ~/Linux/Logs/bash-history-$(date "+%Y-%m-%d").log; fi'

# bash completition
[[ -r "/opt/homebrew/etc/profile.d/bash_completion.sh" ]] && . "/opt/homebrew/etc/profile.d/bash_completion.sh"

# lang
export LANG=en_US.UTF-8

# bash reload
alias bashrl='source ~/.bash_profile'

# nmap scan all
alias nmaps='nmap -sP 192.168.1.0/168'

# cd nmap libs
alias nmlib='cd /usr/local/Cellar/nmap/7.80_1/share/nmap/nselib'
alias nmscrp='cd /usr/local/Cellar/nmap/7.80_1/share/nmap/scripts'

# macchanger
alias maclist='macchanger -s en0'
alias macrand='macchanger -r en0'
alias macdef='macchanger -p en0'

#tor
alias tors='sudo networksetup -setsocksfirewallproxystate Wi-Fi on'
alias tore='sudo networksetup -setsocksfirewallproxystate Wi-Fi off'

# ftp
alias ftp='ncftp'

# backup mariogt.com
alias mariogtbak='sh cloneMarioGTWebsite.sh'

# backup googledrive
alias goobak='sh copyGoo.sh'

# backup onedrive
alias onebak='sh copyOnedrive.sh'

# backup lacie
alias laciebak='sh copyLacie.sh'

# myip
alias ip='curl icanhazip.com'

# top -> htop
alias top='htop'

# cat -> batcat
alias cat='bat --theme ansi'
# man reader -> bat
export MANPAGER="sh -c 'col -bx | bat --theme ansi -l man -p'"

# weather
alias tiempo='ansiweather -l Santiago,CL'
#alias tiempo='curl http://v2.wttr.in'

# corona virus status
alias corona='curl https://corona-stats.online/chile'

export EDITOR=vim
export CLICOLOR=1

# TM Local backups
# delete tmutil deletelocalsnapshots xxxx-xx-xx-xxxxxx
alias tm='sudo tmutil listlocalsnapshots /'
alias tmd='tmutil thinlocalsnapshots / 9999999999 1'

# net Speed Test
alias netspeed='speedtest-cli'

# diskutil list
alias dl='diskutil list'

# cd up
alias cd.='cd ..'
alias cd..='cd ../..'
alias cd...='cd ../../..'

# silence bell
export LESS="$LESS -R -Q"

# sort folder by size
alias dsort='du -m --max-depth 1 | sort -rn'

# bash colors
colorflag="--color"

export LS_COLORS='no=00:fi=00:di=01;32:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:ex=01;31:*.tar=00;31:*.tgz=00;31:*.arj=00;31:*.taz=00;31:*.lzh=00;31:*.zip=00;31:*.z=00;31:*.Z=00;31:*.gz=00;31:*.bz2=00;31:*.deb=00;31:*.rpm=00;31:*.jar=00;31:*.jpg=00;35:*.jpeg=00;35:*.gif=00;35:*.bmp=00;35:*.pbm=00;35:*.pgm=00;35:*.ppm=00;35:*.tga=00;35:*.xbm=00;35:*.xpm=00;35:*.tif=00;35:*.tiff=00;35:*.png=00;35:*.mov=00;35:*.mpg=00;35:*.mpeg=00;35:*.avi=00;35:*.fli=00;35:*.gl=00;35:*.dl=00;35:*.xcf=00;35:*.xwd=00;35:*.ogg=00;35:*.mp3=00;35:*.wav=0;35:*.c=00;32:*.h=00;32:*.cpp=00;36:*.rs=00;36:*.m=00;33:*.xib=00;31:*.swp=02;37:*.docx=00;31:*.doc=00;31:*.xlsx=00;31:*.xls=00;31:*.txt=00;31:*.log=00;33:'

# recursive ls
alias lr='ls -R $PWD/*'

# list all files colorized in long format
alias ll='ls -lHhaFl'

# list all files colorized in long format, including dot files
alias la="ls -lha"

# List only directories
alias lsd='ls -l | grep "^d"'

# clear screen
alias c='clear'

# process
alias ps='ps -ax | more'
# copy
alias cp='cp -iv'                           # Preferred 'cp' implementation
# move
alias mv='mv -iv'                           # Preferred 'mv' implementation
# make dir
alias mkdir='mkdir -pv'                     # Preferred 'mkdir' implementation
# list
alias less='less -FSRXc'                    # Preferred 'less' implementation

# find stuff
alias find='mdfind'

# spotlight: Search for a file using MacOS Spotlight's metadata
# -------------------------------------------------------------
spotlight () { mdfind "kMDItemDisplayName == '$@'wc"; }

# comando para comprimir en zip
zipf () { zip -r "$1".zip "$1" ; }

# find pid
alias pidof='findPid'
findPid () { lsof -t -c "$@" ; }

# ---------------------------
#   6.  NETWORKING
# ---------------------------
alias netCons='lsof -i'                             # netCons:      Show all open TCP/IP sockets
alias flushDNS='dscacheutil -flushcache'            # flushDNS:     Flush out the DNS Cache
alias lsock='sudo /usr/sbin/lsof -i -P'             # lsock:        Display open sockets
alias lsockU='sudo /usr/sbin/lsof -nP | grep UDP'   # lsockU:       Display only open UDP sockets
alias lsockT='sudo /usr/sbin/lsof -nP | grep TCP'   # lsockT:       Display only open TCP sockets
alias ipInfo0='ipconfig getpacket en0'              # ipInfo0:      Get info on connections for en0
alias ipInfo1='ipconfig getpacket en1'              # ipInfo1:      Get info on connections for en1
alias openPorts='sudo lsof -i | grep LISTEN'        # openPorts:    All listening connections
alias showBlocked='sudo ipfw list'                  # showBlocked:  All ipfw rules inc/ blocked IPs

# simple note pad
npad() {
    rememberfile="$HOME/Documentos/remember.txt"

    if [ $# -eq 0 ] ; then
        echo "\nIngresa la nota, para finalizar pulsa ^D: "
        cat - >> $rememberfile
    else
        echo "Today is $(date)" >> $rememberfile
        echo "$@" >> $rememberfile
    fi
}

# net status
rr() {
    echo -e "\n Open Sockets: $NC " ; lsock | less
    echo -e "\n Open Ports: $NC " ; openPorts | less
    echo -e "\n Bloqued by IPFW: $NC " ; showBlocked
}

# ii:  display useful host related informaton
# -------------------------------------------------------------------
ii() {
    echo -e "\nYou are logged on ${RED}$HOST"
    echo -e "\nAdditionnal information:$NC " ; uname -a
    echo -e "\n${RED}Users logged on:$NC " ; w -h
    echo -e "\n${RED}Current date :$NC " ; date
    echo -e "\n${RED}Machine stats :$NC " ; uptime
    echo -e "\n${RED}Current network location :$NC " ; scselect
    echo
}

# # extract multiple compression formats
extract () {
    if [ -f $1 ]; then
        case $1 in
            *.tar.bz2) tar xjf $1;;
            *.tar.gz) tar xzf $1;;
            *.bz2) bunzip2 $1;;
            *.rar) unrar e $1;;
            *.gz) gunzip $1;;
            *.tar) tar xf $1;;
            *.tbz2) tar xjf $1;;
            *.tgz) tar xzf $1;;
            *.zip) unzip $1;;
            *.Z) uncompress $1;;
            *.7z) 7z x $1;;
            *) echo "'$1' no se puede extraer via extract()";;
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
# git pull
alias gitpull='sh gitpull.sh'

# alias dev dirs
alias cdrust='cd ~/code/sourceCode/rust'
alias cdgit='cd ~/code/sourceCode/githubMix/'
alias cdcc='cd ~/code/sourceCode/currentCode/'
alias cdcode='cd ~/code/sourceCode/'

# emacs tags
# plataformas para los SDKs de apple
#AppleTVOS.platform/
#AppleTVSimulator.platform
#MacOSX.platform/
#WatchOS.platform/
#WatchSimulator.platform/
#iPhoneOS.platform/
#iPhoneSimulator.platform/

# genera tags de todos los SDKs macOS / brew / y otras librerias necesarias
# lento pero generalmente se utiliza una sola vez a menos que cambien los SDKs
alias tags='ctags --exclude="*/build/*" --exclude="build/*" --exclude="*/.git/*" --exclude=".git/*" --exclude="*/.svn/*" --exclude=".svn/*" --exclude="*/.cvs/*" --exclude=".cvs/*" --exclude="*/.bzr/*" --exclude=".bzr/*" --exclude="*/.hg/*" --exclude=".hg/*" --exclude="*/bin/*" --exclude="bin/*" --exclude="*/fonts/*" --exclude="fonts/*" --exclude="*/images/*" --exclude="images/*" --exclude="*/.DS_Store/*" --exclude=".DS_Store/*" --exclude="*/.npm/*" --exclude=".npm/*" --exclude="*/.tmp/*" --exclude=".tmp/*" --exclude="*/.sass-cache/*" --exclude=".sass-cache/*" --exclude="*/.idea/*" --exclude=".idea/*" --exclude="*/node_modules/*" --exclude="node_modules/*" --exclude="*/bower_components/*" --exclude="bower_components/*" --exclude="*/.tox/*" --exclude=".tox/*" --exclude="*/.vscode/*" --exclude=".vscode/*" --exclude="*/.cask/*" --exclude=".cask/*" --exclude="*.log" --exclude="rusty-tags.vim" --exclude="rusty-tags.emacs" --exclude="tags" --exclude="TAGS" --exclude="*.tgz" --exclude="*.gz" --exclude="*.xz" --exclude="*.zip" --exclude="*.tar" --exclude="*.rar" --exclude="GTAGS" --exclude="GPATH" --exclude="GRTAGS" --exclude="cscope.files" --exclude="*bundle.js" --exclude="*min.js" --exclude="*min.css" --exclude="*.png" --exclude="*.jpg" --exclude="*.jpeg" --exclude="*.gif" --exclude="*.bmp" --exclude="*.tiff" --exclude="*.ico" --exclude="*.doc" --exclude="*.docx" --exclude="*.xls" --exclude="*.ppt" --exclude="*.pdf" --exclude="*.odt" --exclude=".clang-format" --exclude="*.obj" --exclude="*.so" --exclude="*.o" --exclude="*.a" --exclude="*.ifso" --exclude="*.tbd" --exclude="*.dylib" --exclude="*.lib" --exclude="*.d" --exclude="*.dll" --exclude="*.exe" --exclude=".metadata*" --exclude="*.class" --exclude="*.war" --exclude="*.jar" --exclude="*flymake" --exclude="#*#" --exclude=".#*" --exclude="*.swp" --exclude="*~" --exclude="*.3pm" --exclude="*.elc" --exclude="*.pyc" --tag-relative=no -e -R *'

# genera tags de todos los SDKs macOS / brew / y otras librerias necesarias
# lento pero generalmente se utiliza una sola vez a menos que cambien los SDKs
alias macossdktags='ctags --exclude="*/build/*" --exclude="build/*" --exclude="*/.git/*" --exclude=".git/*" --exclude="*/.svn/*" --exclude=".svn/*" --exclude="*/.cvs/*" --exclude=".cvs/*" --exclude="*/.bzr/*" --exclude=".bzr/*" --exclude="*/.hg/*" --exclude=".hg/*" --exclude="*/bin/*" --exclude="bin/*" --exclude="*/fonts/*" --exclude="fonts/*" --exclude="*/images/*" --exclude="images/*" --exclude="*/.DS_Store/*" --exclude=".DS_Store/*" --exclude="*/.npm/*" --exclude=".npm/*" --exclude="*/.tmp/*" --exclude=".tmp/*" --exclude="*/.sass-cache/*" --exclude=".sass-cache/*" --exclude="*/.idea/*" --exclude=".idea/*" --exclude="*/node_modules/*" --exclude="node_modules/*" --exclude="*/bower_components/*" --exclude="bower_components/*" --exclude="*/.tox/*" --exclude=".tox/*" --exclude="*/.vscode/*" --exclude=".vscode/*" --exclude="*/.cask/*" --exclude=".cask/*" --exclude="*.log" --exclude="rusty-tags.vim" --exclude="rusty-tags.emacs" --exclude="tags" --exclude="TAGS" --exclude="*.tgz" --exclude="*.gz" --exclude="*.xz" --exclude="*.zip" --exclude="*.tar" --exclude="*.rar" --exclude="GTAGS" --exclude="GPATH" --exclude="GRTAGS" --exclude="cscope.files" --exclude="*bundle.js" --exclude="*min.js" --exclude="*min.css" --exclude="*.png" --exclude="*.jpg" --exclude="*.jpeg" --exclude="*.gif" --exclude="*.bmp" --exclude="*.tiff" --exclude="*.ico" --exclude="*.doc" --exclude="*.docx" --exclude="*.xls" --exclude="*.ppt" --exclude="*.pdf" --exclude="*.odt" --exclude=".clang-format" --exclude="*.obj" --exclude="*.so" --exclude="*.o" --exclude="*.a" --exclude="*.ifso" --exclude="*.tbd" --exclude="*.dylib" --exclude="*.lib" --exclude="*.d" --exclude="*.dll" --exclude="*.exe" --exclude=".metadata*" --exclude="*.class" --exclude="*.war" --exclude="*.jar" --exclude="*flymake" --exclude="#*#" --exclude=".#*" --exclude="*.swp" --exclude="*~" --exclude="*.3pm" --exclude="*.elc" --exclude="*.pyc" --tag-relative=no -e -R /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/ /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/System/Library/ /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/12.0.5/include/ /opt/homebrew/include/'

# genera tags de proyectos macOS
# incluye tags maestros macossdktags
alias macostags='ctags --exclude="*/build/*" --exclude="build/*" --exclude="*/.git/*" --exclude=".git/*" --exclude="*/.svn/*" --exclude=".svn/*" --exclude="*/.cvs/*" --exclude=".cvs/*" --exclude="*/.bzr/*" --exclude=".bzr/*" --exclude="*/.hg/*" --exclude=".hg/*" --exclude="*/bin/*" --exclude="bin/*" --exclude="*/fonts/*" --exclude="fonts/*" --exclude="*/images/*" --exclude="images/*" --exclude="*/.DS_Store/*" --exclude=".DS_Store/*" --exclude="*/.npm/*" --exclude=".npm/*" --exclude="*/.tmp/*" --exclude=".tmp/*" --exclude="*/.sass-cache/*" --exclude=".sass-cache/*" --exclude="*/.idea/*" --exclude=".idea/*" --exclude="*/node_modules/*" --exclude="node_modules/*" --exclude="*/bower_components/*" --exclude="bower_components/*" --exclude="*/.tox/*" --exclude=".tox/*" --exclude="*/.vscode/*" --exclude=".vscode/*" --exclude="*/.cask/*" --exclude=".cask/*" --exclude="*.log" --exclude="rusty-tags.vim" --exclude="rusty-tags.emacs" --exclude="tags" --exclude="TAGS" --exclude="*.tgz" --exclude="*.gz" --exclude="*.xz" --exclude="*.zip" --exclude="*.tar" --exclude="*.rar" --exclude="GTAGS" --exclude="GPATH" --exclude="GRTAGS" --exclude="cscope.files" --exclude="*bundle.js" --exclude="*min.js" --exclude="*min.css" --exclude="*.png" --exclude="*.jpg" --exclude="*.jpeg" --exclude="*.gif" --exclude="*.bmp" --exclude="*.tiff" --exclude="*.ico" --exclude="*.doc" --exclude="*.docx" --exclude="*.xls" --exclude="*.ppt" --exclude="*.pdf" --exclude="*.odt" --exclude=".clang-format" --exclude="*.obj" --exclude="*.so" --exclude="*.o" --exclude="*.a" --exclude="*.ifso" --exclude="*.tbd" --exclude="*.dylib" --exclude="*.lib" --exclude="*.d" --exclude="*.dll" --exclude="*.exe" --exclude=".metadata*" --exclude="*.class" --exclude="*.war" --exclude="*.jar" --exclude="*flymake" --exclude="#*#" --exclude=".#*" --exclude="*.swp" --exclude="*~" --exclude="*.elc" --exclude="*.pyc" --tag-relative=no -e -R --etags-include="~/code/sourceCode/currentCode/etags/macOS/TAGS" *'

# genera tags de SDKs para iOS
# lento pero generalmente se utiliza una sola vez a menos que cambien los SDKs
alias iossdktags='ctags --exclude="*/build/*" --exclude="build/*" --exclude="*/.git/*" --exclude=".git/*" --exclude="*/.svn/*" --exclude=".svn/*" --exclude="*/.cvs/*" --exclude=".cvs/*" --exclude="*/.bzr/*" --exclude=".bzr/*" --exclude="*/.hg/*" --exclude=".hg/*" --exclude="*/bin/*" --exclude="bin/*" --exclude="*/fonts/*" --exclude="fonts/*" --exclude="*/images/*" --exclude="images/*" --exclude="*/.DS_Store/*" --exclude=".DS_Store/*" --exclude="*/.npm/*" --exclude=".npm/*" --exclude="*/.tmp/*" --exclude=".tmp/*" --exclude="*/.sass-cache/*" --exclude=".sass-cache/*" --exclude="*/.idea/*" --exclude=".idea/*" --exclude="*/node_modules/*" --exclude="node_modules/*" --exclude="*/bower_components/*" --exclude="bower_components/*" --exclude="*/.tox/*" --exclude=".tox/*" --exclude="*/.vscode/*" --exclude=".vscode/*" --exclude="*/.cask/*" --exclude=".cask/*" --exclude="*.log" --exclude="rusty-tags.vim" --exclude="rusty-tags.emacs" --exclude="tags" --exclude="TAGS" --exclude="*.tgz" --exclude="*.gz" --exclude="*.xz" --exclude="*.zip" --exclude="*.tar" --exclude="*.rar" --exclude="GTAGS" --exclude="GPATH" --exclude="GRTAGS" --exclude="cscope.files" --exclude="*bundle.js" --exclude="*min.js" --exclude="*min.css" --exclude="*.png" --exclude="*.jpg" --exclude="*.jpeg" --exclude="*.gif" --exclude="*.bmp" --exclude="*.tiff" --exclude="*.ico" --exclude="*.doc" --exclude="*.docx" --exclude="*.xls" --exclude="*.ppt" --exclude="*.pdf" --exclude="*.odt" --exclude=".clang-format" --exclude="*.obj" --exclude="*.so" --exclude="*.o" --exclude="*.a" --exclude="*.ifso" --exclude="*.tbd" --exclude="*.dylib" --exclude="*.lib" --exclude="*.d" --exclude="*.dll" --exclude="*.exe" --exclude=".metadata*" --exclude="*.class" --exclude="*.war" --exclude="*.jar" --exclude="*flymake" --exclude="#*#" --exclude=".#*" --exclude="*.swp" --exclude="*~" --exclude="*.elc" --exclude="*.pyc" --exclude="*.3pm" --tag-relative=no -e -R /Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk/usr/include/ /Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator.sdk/usr/include/ /Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator.sdk/System/Library/ /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/12.0.5/include/ /opt/homebrew/include/'

# genera tags de proyectos iOS
# incluye tags maestros iosdktags
alias iostags='ctags --exclude="*/build/*" --exclude="build/*" --exclude="*/.git/*" --exclude=".git/*" --exclude="*/.svn/*" --exclude=".svn/*" --exclude="*/.cvs/*" --exclude=".cvs/*" --exclude="*/.bzr/*" --exclude=".bzr/*" --exclude="*/.hg/*" --exclude=".hg/*" --exclude="*/bin/*" --exclude="bin/*" --exclude="*/fonts/*" --exclude="fonts/*" --exclude="*/images/*" --exclude="images/*" --exclude="*/.DS_Store/*" --exclude=".DS_Store/*" --exclude="*/.npm/*" --exclude=".npm/*" --exclude="*/.tmp/*" --exclude=".tmp/*" --exclude="*/.sass-cache/*" --exclude=".sass-cache/*" --exclude="*/.idea/*" --exclude=".idea/*" --exclude="*/node_modules/*" --exclude="node_modules/*" --exclude="*/bower_components/*" --exclude="bower_components/*" --exclude="*/.tox/*" --exclude=".tox/*" --exclude="*/.vscode/*" --exclude=".vscode/*" --exclude="*/.cask/*" --exclude=".cask/*" --exclude="*.log" --exclude="rusty-tags.vim" --exclude="rusty-tags.emacs" --exclude="tags" --exclude="TAGS" --exclude="*.tgz" --exclude="*.gz" --exclude="*.xz" --exclude="*.zip" --exclude="*.tar" --exclude="*.rar" --exclude="GTAGS" --exclude="GPATH" --exclude="GRTAGS" --exclude="cscope.files" --exclude="*bundle.js" --exclude="*min.js" --exclude="*min.css" --exclude="*.png" --exclude="*.jpg" --exclude="*.jpeg" --exclude="*.gif" --exclude="*.bmp" --exclude="*.tiff" --exclude="*.ico" --exclude="*.doc" --exclude="*.docx" --exclude="*.xls" --exclude="*.ppt" --exclude="*.pdf" --exclude="*.odt" --exclude=".clang-format" --exclude="*.obj" --exclude="*.so" --exclude="*.o" --exclude="*.a" --exclude="*.ifso" --exclude="*.tbd" --exclude="*.dylib" --exclude="*.lib" --exclude="*.d" --exclude="*.dll" --exclude="*.exe" --exclude=".metadata*" --exclude="*.class" --exclude="*.war" --exclude="*.jar" --exclude="*flymake" --exclude="#*#" --exclude=".#*" --exclude="*.swp" --exclude="*~" --exclude="*.elc" --exclude="*.pyc" --tag-relative=no -e -R --etags-include="~/code/sourceCode/currentCode/etags/iOS/TAGS" *'

# genera tags de proyectos raylib
alias raytags='ctags --exclude="*/build/*" --exclude="build/*" --exclude="*/.git/*" --exclude=".git/*" --exclude="*/.svn/*" --exclude=".svn/*" --exclude="*/.cvs/*" --exclude=".cvs/*" --exclude="*/.bzr/*" --exclude=".bzr/*" --exclude="*/.hg/*" --exclude=".hg/*" --exclude="*/bin/*" --exclude="bin/*" --exclude="*/fonts/*" --exclude="fonts/*" --exclude="*/images/*" --exclude="images/*" --exclude="*/.DS_Store/*" --exclude=".DS_Store/*" --exclude="*/.npm/*" --exclude=".npm/*" --exclude="*/.tmp/*" --exclude=".tmp/*" --exclude="*/.sass-cache/*" --exclude=".sass-cache/*" --exclude="*/.idea/*" --exclude=".idea/*" --exclude="*/node_modules/*" --exclude="node_modules/*" --exclude="*/bower_components/*" --exclude="bower_components/*" --exclude="*/.tox/*" --exclude=".tox/*" --exclude="*/.vscode/*" --exclude=".vscode/*" --exclude="*/.cask/*" --exclude=".cask/*" --exclude="*.log" --exclude="rusty-tags.vim" --exclude="rusty-tags.emacs" --exclude="tags" --exclude="TAGS" --exclude="*.tgz" --exclude="*.gz" --exclude="*.xz" --exclude="*.zip" --exclude="*.tar" --exclude="*.rar" --exclude="GTAGS" --exclude="GPATH" --exclude="GRTAGS" --exclude="cscope.files" --exclude="*bundle.js" --exclude="*min.js" --exclude="*min.css" --exclude="*.png" --exclude="*.jpg" --exclude="*.jpeg" --exclude="*.gif" --exclude="*.bmp" --exclude="*.tiff" --exclude="*.ico" --exclude="*.doc" --exclude="*.docx" --exclude="*.xls" --exclude="*.ppt" --exclude="*.pdf" --exclude="*.odt" --exclude=".clang-format" --exclude="*.obj" --exclude="*.so" --exclude="*.o" --exclude="*.a" --exclude="*.ifso" --exclude="*.tbd" --exclude="*.dylib" --exclude="*.lib" --exclude="*.d" --exclude="*.dll" --exclude="*.exe" --exclude=".metadata*" --exclude="*.class" --exclude="*.war" --exclude="*.jar" --exclude="*flymake" --exclude="#*#" --exclude=".#*" --exclude="*.swp" --exclude="*~" --exclude="*.elc" --exclude="*.pyc" --tag-relative=no -e -R ~/code/sourceCode/githubMix/raylib/src/ *'

# rust Dev
export RUST_SRC_PATH="/Users/mario/.rustup/toolchains/nightly-aarch64-apple-darwin/lib/rustlib/src/rust/src/"
. "$HOME/.cargo/env"

# alias cargo Rust
alias ccc='cargo clean'
alias ccb='cargo build'
alias ccr='cargo run'
alias cct='cargo test'
alias rustag='rusty-tags emacs'

# brew
eval "$(/opt/homebrew/bin/brew shellenv)"
export LIBRARY_PATH="$LIBRARY_PATH:/opt/homebrew/lib/"
export GUILE_LOAD_PATH="/opt/homebrew/share/guile/site/3.0"
export GUILE_LOAD_COMPILED_PATH="/opt/homebrew/lib/guile/3.0/site-ccache"
export GUILE_SYSTEM_EXTENSIONS_PATH="/opt/homebrew/lib/guile/3.0/extensions"

export LDFLAGS="-L/opt/homebrew/opt/llvm/lib"
export CPPFLAGS="-I/opt/homebrew/opt/llvm/include"

# paths
export PATH=$PATH:~/code/sourceCode/scripts/
export PATH="/opt/homebrew/opt/llvm/bin:$PATH"
export PATH="/opt/homebrew/opt/ruby/bin:$PATH"
export PATH="/opt/homebrew/lib/ruby/gems/3.0.0/bin:$PATH"
export PATH="/Users/mario/code/sourceCode/githubMix/limelight/bin:$PATH"
export PATH="/Users/mario/code/sourceCode/githubMix/yabai/bin:$PATH"
export PATH="/opt/homebrew/bin:$PATH"
