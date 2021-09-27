# -*- mode: sh -*-

# function zvm_before_init() {
#     # fixes issue with zsh-autosuggestions no longer
#     # working in `zsh-vi-mode`
#     #
#     # https://github.com/jeffreytse/zsh-vi-mode/issues/15#issuecomment-790243410
#     zvm_bindkey viins '^[[A' history-beginning-search-backward
#     zvm_bindkey viins '^[[B' history-beginning-search-forward
#     zvm_bindkey vicmd '^[[A' history-beginning-search-backward
#     zvm_bindkey vicmd '^[[B' history-beginning-search-forward
# }

vterm_printf(){
    # Some of the most useful features in vterm
    # (e.g., directory-tracking and prompt-tracking or message passing)
    # require shell-side configurations. The main goal of these additional
    # functions is to enable the shell to send information to vterm via properly
    # escaped sequences. A function that helps in this task, vterm_printf, is defined below.
    # This function is widely used throughout this readme.

    if [ -n "$TMUX" ]; then
        # Tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

my-local-ip() {
    ifconfig en0 | grep "inet " | awk '{print $2}'
}

open-tunnel() {
    TUNNEL_NAME=$1
    TUNNEL_LOCAL_PORT=$2
    TUNNEL_ADDR=$3
    TUNNEL_LOGIN=$4
    ssh -i ~/.ssh/id_rsa -M -S $TUNNEL_NAME-tunnel -fnNT -L 127.0.0.1:$TUNNEL_LOCAL_PORT:$TUNNEL_ADDR -N $TUNNEL_LOGIN
}

close-tunnel() {
    TUNNEL_NAME=$1
    TUNNEL_LOGIN=$2
    ssh -i ~/.ssh/id_rsa -S $TUNNEL_NAME-tunnel -O exit $TUNNEL_LOGIN
}


# Useful for appending headers to large files
append-header() {
    echo $1 | cat - $2 > temp && mv temp $2
}


now() {
    date +%y%m%d%H%M%S
}

tblexport(){
    psql -d $1 -U postgres -h localhost -p $2 -c "COPY $3 TO STDOUT WITH CSV HEADER" > $4.csv
}

search() {
    google() {
        open "https://www.google.com/search?q=$1"
    }

    soundcloud() {
        open "https://soundcloud.com/search?q=$1"
    }

    github() {
        open "https://github.com/search?q=$1"
    }

    restaurants() {
        for restaurant in "$@"; do
            search google "$restaurant yelp"
            search google "$restaurant foursquare"
        done
    }

    youtube() {
        open "https://www.youtube.com/results?search_query=$1"
    }

    enigma-app() {
        open "https://app.enigma.io/search/$1"
    }
$@
}

dockerpsrm() {
    docker rm -f $(docker ps -a | awk '{ if ($1 != "CONTAINER") print $1}')
}

dockerimagerm() {
    docker rmi $(docker images | awk '{ if ($1 == "<none>") print $3 }')
}

resolve() {
    host $1 | awk '{ print $4  }' | grep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn} -v 'in' | xargs -I {} dig +short -x {}
}

install_or_update_python_ide_libs() {
	#flake8 
	#black 
	#jedi 
	#isort 
	#pyls-black 
     pip install -U pip ipython flake8 black isort pytest nose pyflakes rope importmagic pyls-flake8 pyls-black pyls-isort "python-lsp-server[all]" --force-reinstall --use-feature=2020-resolver
}
 
# Path to your oh-my-zsh installation.
export ZSH="/Users/sesshin/.oh-my-zsh"

# needs to load before the source oh-my-zsh.sh
ZSH_THEME="eastwood"
COMPLETION_WAITING_DOTS="true"
HIST_STAMPS="mm/dd/yyyy"

#plugins=(zsh-vi-mode history-substring-search colored-man-pages)
plugins=(history-substring-search colored-man-pages pyenv)
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


# golang setup
export GOPATH="${HOME}/go"
export GOROOT="/usr/local/opt/go/libexec"
export PATH="$PATH:${GOPATH}/bin:${GOROOT}/bin"
export PATH="/usr/local/sbin:$PATH"

alias pgstart="postgres -D /usr/local/var/postgres"
alias ip="ipython"
alias pss="ps aux | grep -i $1"
alias pup="pip install -U pip"
alias lsl="ls -l"
alias lsal="ls -al"
alias tidy=/usr/local/bin/tidy
alias emacsdaemon="emacs --daemon"


# https://stackoverflow.com/questions/50168647/multiprocessing-causes-python-to-crash-and-gives-an-error-may-have-been-in-progr/52230415#52230415
export OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES

#export PYENV_ROOT="$HOME/.pyenv"
#export PATH="$PYENV_ROOT/bin:$PATH"
#export PYENV_VIRTUALENVWRAPPER_PREFER_PYVENV="true"

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init --path)"

if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init --path)"
fi

#if which pyenv-virtualenv-init > /dev/null; then eval "$(pyenv virtualenv-init -)"; fi

source "$HOME/.cargo/env"
#ZVM_VI_INSERT_ESCAPE_BINDKEY=jk

ZSH_PYENV_VIRTUALENV=true

