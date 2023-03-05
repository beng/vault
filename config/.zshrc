# -*- mode: sh -*-

zmodload zsh/zprof

# Path to your oh-my-zsh installation.
export ZSH="/Users/sesshin/.oh-my-zsh"

# needs to load before the source oh-my-zsh.sh
ZSH_THEME="eastwood"
COMPLETION_WAITING_DOTS="true"
HIST_STAMPS="yyyy-mm-dd"

plugins=(history-substring-search colored-man-pages)
source $HOME/.zshrc.functions
source "$HOME/.cargo/env"
source $ZSH/oh-my-zsh.sh
source ~/.inputrc

# https://github.com/syl20bnr/spacemacs/wiki/Terminal
export TERM=xterm-24bit
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8

export PATH="$HOME/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/local/sbin:/opt/X11/bin:/usr/texbin"
export PATH=/usr/local/bin:$PATH
export PATH="$HOME/.emacs.d/bin:$PATH"
export PATH="$HOME/bin:$PATH"
export PATH="$HOME/.config/emacs/bin:$PATH"

# golang setup
export GOPATH="${HOME}/go"
export GOROOT="/usr/local/opt/go/libexec"
export PATH="$PATH:${GOPATH}/bin:${GOROOT}/bin"
export PATH="/usr/local/sbin:$PATH"

export LDFLAGS="-L/usr/local/opt/openblas/lib"
export CPPFLAGS="-I/usr/local/opt/openblas/include"

alias pgstart="postgres -D /usr/local/var/postgres"
alias ip="ipython"
alias pss="ps aux | grep -i $1"
alias pup="pip install -U pip"
alias lsl="ls -l"
alias lsal="ls -al"
alias tidy=/usr/local/bin/tidy
alias ed="emacs --daemon=emacs-daemon"
alias ec="emacsclient --socket-name=emacs-daemon -nw -c"

# shell is using 24bit color which breaks SSH sessions, this will
# set the TERM when sshing into a server
alias ssh='TERM="xterm-256color" ssh'

# https://stackoverflow.com/questions/50168647/multiprocessing-causes-python-to-crash-and-gives-an-error-may-have-been-in-progr/52230415#52230415
export OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES

# loads pdflatex
eval "$(/usr/libexec/path_helper)"

# pyenv installation
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
# if problems start to arise, go back to using default command `pyenv init -`
eval "$(pyenv init --path)"
# this command is faster, but forces manually rehashing
#eval "$(pyenv init --path --no-rehash)"

ZSH_THEME_TERM_TITLE_IDLE="%3~"
ZSH_THEME_TERM_TAB_TITLE_IDLE="%3~"

# https://gist.github.com/ctechols/ca1035271ad134841284
#
# On slow systems, checking the cached .zcompdump file to see if it must be
# regenerated adds a noticable delay to zsh startup.  This little hack restricts
# it to once a day.  It should be pasted into your own completion file.
#
# The globbing is a little complicated here:
# - '#q' is an explicit glob qualifier that makes globbing work within zsh's [[ ]] construct.
# - 'N' makes the glob pattern evaluate to nothing when it doesn't match (rather than throw a globbing error)
# - '.' matches "regular files"
# - 'mh+24' matches files (or directories or whatever) that are older than 24 hours.
autoload -Uz compinit
if [[ -n ${ZDOTDIR}/.zcompdump(#qN.mh+24) ]]; then
	compinit;
else
	compinit -C;
fi;

PROMPT='%{$fg[yellow]%}[ %D{%L:%M:%S} ] '$PROMPT
