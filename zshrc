ZSH=$HOME/.oh-my-zsh
ZSH_THEME="fishy"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git)

source $ZSH/oh-my-zsh.sh

# vi keys
#function zle-line-init zle-keymap-select {
#	RPS1="${${KEYMAP/vicmd/-- NORMAL --}/(main|viins)/-- INSERT --}"
#	RPS2=$RPS1
#	zle reset-prompt
#}
#
#zle -N zle-line-init
#zle -N zle-keymap-select
#set -o vi
# vi keys end

# Customize to your needs...

export EDITOR="emacsclient -a vim"
alias E="emacsclient -t -a vim"
export COLORFGBG="default;default;0"
export LANG="en_US"
export LC_CTYPE="en_US"
