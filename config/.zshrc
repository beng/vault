open_tunnel() {
    TUNNEL_NAME=$1
    TUNNEL_LOCAL_PORT=$2
    TUNNEL_ADDR=$3
    TUNNEL_LOGIN=$4
    ssh -i ~/.ssh/id_rsa -M -S $TUNNEL_NAME-tunnel -fnNT -L 127.0.0.1:$TUNNEL_LOCAL_PORT:$TUNNEL_ADDR -N $TUNNEL_LOGIN
}

close_tunnel() {
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

    enigma-api() {
        root="https://api.enigma.io/v2/$type/$ENIGMA_API_TOKEN/$2"
        data() {
            type="data"
        }

        meta() {
            type="meta"
        }

        $@
        curl $root | jq '.'
    }

    $@
}

dockerpsrm() {
    docker rm -f $(docker ps -a | awk '{ if ($1 != "CONTAINER") print $1}')
}

dockerimagerm() {
    docker rmi $(docker images | awk '{ if ($1 == "<none>") print $3 }')
}


# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="minimal"
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
    vi-mode \
    colorize \
    colored-man-pages)

source $ZSH/oh-my-zsh.sh

export PATH="$HOME/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/local/sbin:/opt/X11/bin:/usr/texbin"
export PATH=/usr/local/bin:$PATH
export RC=$HOME/.zshrc
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:/usr/local/opt/go/libexec/bin
export PATH=$PATH:$HOME/.cargo/bin

alias pgstart="postgres -D /usr/local/var/postgres"
alias pys="python -m SimpleHTTPServer"
alias ip="ipython"
alias pss="ps aux | grep -i $1"
alias pup="pip install -U pip"

# OPAM configuration
. $HOME/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
