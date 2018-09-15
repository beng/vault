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

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="lambda"
COMPLETION_WAITING_DOTS="true"
HIST_STAMPS="mm/dd/yyyy"
plugins=(git \
    pip \
    brew \
    cabal \
    lein \
    osx \
    zsh-syntax-highlighting \
    history-substring-search \
    colorize \
    colored-man-pages)

source $ZSH/oh-my-zsh.sh

export PATH="$HOME/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/local/sbin:/opt/X11/bin:/usr/texbin:/usr/local/terraform-0.8.6"
export PATH=/usr/local/bin:$PATH
export RC=$HOME/.zshrc
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:/usr/local/opt/go/libexec/bin
export PATH=$PATH:$HOME/.cargo/bin
export PATH=$PATH:$GOPATH/src
export PATH="$HOME/.cargo/bin:$PATH"

alias pgstart="postgres -D /usr/local/var/postgres"
alias pys="python -m SimpleHTTPServer"
alias ip="ipython"
alias pss="ps aux | grep -i $1"
alias pup="pip install -U pip"
alias clear="clear && printf '\e[3J'"

alias bibliotech="~/Documents/work/bibliotech"

# OPAM configuration
. $HOME/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

# node6 bs
export PATH="/usr/local/opt/node@6/bin:$PATH"

# pyenv setup
if command -v pyenv 1>/dev/null 2>&1; then
    eval "$(pyenv init -)"
fi
if which pyenv-virtualenv-init > /dev/null; then eval "$(pyenv virtualenv-init -)"; fi
#export PYENV_VIRTUALENV_DISABLE_PROMPT=1

source ~/.inputrc

export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"

if [ -f ~/.enigma ]; then
    source ~/.enigma
fi
