eval "$(pyenv init -)"
#eval "$(pyenv init - --no-rehash)"
#eval "$(pyenv virtualenv-init - --no-rehash)"
#eval $(opam config env)

export PATH="$PATH:$HOME/.cargo/bin:$PATH"

# golang setup
export GOPATH="${HOME}/go"
export GOROOT="/usr/local/opt/go/libexec"
export PATH="$PATH:${GOPATH}/bin:${GOROOT}/bin"
export PATH="/usr/local/sbin:$PATH"
export PATH="$HOME/.emacs.d/bin:$PATH"
export PATH="$HOME/bin:$PATH"

install_or_update_python_tools() {
     pip install -U pip pytest nose black pyflakes isort "python-language-server[all]" --force-reinstall
     #pip install -U pip black rope jedi importmagic autopep8 flake8 "python-language-server[all]"
}

alias lsl="ls -l"
alias lsal="ls -al"

source ~/.enigma.env
