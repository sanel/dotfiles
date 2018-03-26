PS1="[\u@\h \W]\\$ "
alias E="emacsclient -t -a vi"
alias fsl="fossil"
export EDITOR="emacsclient -a vi"
export COLORFGBG="default:default:0"
export LANG="en_US.utf8"
export LC_TYPE="en_US.utf8"
export _JAVA_OPTIONS="-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dsun.java2d.opengl=true"
export PATH="$PATH:~/programs/firefox:/opt/tor/bin"

# directory color
LS_COLORS=$LS_COLORS:'di=0;31:' ; export LS_COLORS

# add color support
export LESS="-R"
alias ls="ls --color"

# from http://boredzo.org/blog/archives/2016-08-15/colorized-man-pages-understood-and-customized
man() {
    env LESS_TERMCAP_mb=$'\e[1;31m' \
        LESS_TERMCAP_md=$'\e[1;31m' \
        LESS_TERMCAP_me=$'\e[0m' \
        LESS_TERMCAP_se=$'\e[0m' \
        LESS_TERMCAP_so=$'\e[1;44;33m' \
        LESS_TERMCAP_ue=$'\e[0m' \
        LESS_TERMCAP_us=$'\e[1;32m' \
        man "$@"
}
