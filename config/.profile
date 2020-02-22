export PATH=$PATH:$HOME/.local/bin

# opam configuration
test -r /Users/sesshin/.opam/opam-init/init.sh && . /Users/sesshin/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
eval `opam config env`

#export PATH="$HOME/.cargo/bin:$PATH"

