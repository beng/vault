eval "$(/usr/libexec/path_helper)"
eval "$(/opt/homebrew/bin/brew shellenv)"
eval "$(/opt/homebrew/bin/mise activate zsh --shims)"
eval "$(/opt/homebrew/bin/mise activate zsh)"

# https://github.com/syl20bnr/spacemacs/wiki/Terminal
export TERM=xterm-24bit
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8

export GOPATH="${HOME}/go"
export GOROOT="/usr/local/opt/go/libexec"

export LDFLAGS="-L/usr/local/opt/openblas/lib"
export CPPFLAGS="-I/usr/local/opt/openblas/include"
export LIBRARY_PATH="/opt/homebrew/opt/gcc/lib/gcc/current:/opt/homebrew/opt/libgccjit/lib/gcc/current:/opt/homebrew/opt/gcc/lib/gcc/current/gcc/aarch64-apple-darwin23/13"

export PATH="$PATH:$HOME/bin:$HOME/.emacs.d/bin:$HOME/.config/emacs/bin:$HOME/.cargo/bin:/Applications/kitty.app/Contents/MacOS:$GOPATH/bin:$GOROOT/bin"

# export PATH=/usr/local/bin:$PATH
# export PATH="$HOME/.emacs.d/bin:$PATH"
# export PATH="$HOME/bin:$PATH"
# export PATH="$HOME/.config/emacs/bin:$PATH"
# export PATH="$HOME/.cargo/bin:$PATH"
# export PATH="/Applications/kitty.app/Contents/MacOS:$PATH"
# export PATH="/usr/local/sbin:$PATH"
# export PATH="$HOME/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/local/sbin:/opt/X11/bin:/usr/texbin"
