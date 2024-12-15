# -*- mode: sh -*-

# zmodload zsh/zprof

# Path to your oh-my-zsh installation.
export ZSH="/Users/ben/.oh-my-zsh"

# needs to load before the source oh-my-zsh.sh
ZSH_THEME="eastwood"
COMPLETION_WAITING_DOTS="true"
HIST_STAMPS="yyyy-mm-dd"

plugins=(
	zsh-autosuggestions
	history-substring-search
	colored-man-pages
)

source $HOME/.zshrc.functions
#source "$HOME/.cargo/env"
source $ZSH/oh-my-zsh.sh
source ~/.inputrc

# Enable substitution in the prompt.
setopt prompt_subst
# custom prompt instead of the theme prompt
#PROMPT='%{$fg[yellow]%}[ %D{%L:%M:%S} ] %{$fg[white]%}$(git_branch_name) %{$fg[cyan]%}[%~% ]%{$reset_color%}%B$%b '
NEWLINE=$'\n'
PROMPT='%{$fg[cyan]%}${NEWLINE}[ %(5~|%-1~/…/%3~|%4~) ] %{$fg[white]%}$(git_branch_name)%{$reset_color%}${NEWLINE}%B> %b '


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
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="/Applications/kitty.app/Contents/MacOS:$PATH"

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
alias ec_gui="emacsclient --socket-name=emacs-daemon -c & disown"
alias arena="cd ~/Documents/code/arena-ai/"
alias code="cd ~/Documents/code/"
alias vim="nvim"

# shell is using 24bit color which breaks SSH sessions, this will
# set the TERM when sshing into a server
alias ssh='TERM="xterm-256color" ssh'

# https://stackoverflow.com/questions/50168647/multiprocessing-causes-python-to-crash-and-gives-an-error-may-have-been-in-progr/52230415#52230415
export OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES

# loads pdflatex
eval "$(/usr/libexec/path_helper)"

# pyenv installation
#export PYENV_ROOT="$HOME/.pyenv"
#command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
#export PYENV_VIRTUALENV_DISABLE_PROMPT=1
# if problems start to arise, go back to using default command `pyenv init -`
#eval "$(pyenv init --path)"
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

export PATH="/opt/homebrew/bin:$PATH"
export PATH="/opt/homebrew/sbin:$PATH"

# [ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
eval "$(fzf --zsh)"

export FZF_DEFAULT_OPTS="
    --multi --cycle --keep-right -1 \
    --height=40% --layout=reverse --info=inline \
    --preview-window right:70% \
    --preview '[[ \$(file --mime {}) =~ binary ]] && echo {} is a binary file || (bat --style=numbers --color=always {} || cat {}) 2>/dev/null | head -300' \
    --bind='ctrl-a:select-all+accept,ctrl-y:execute-silent(echo {+} | pbcopy)' \
    --ansi"
# export FZF_DEFAULT_OPTS="--height 40% --layout reverse --info inline --border \
#     --preview 'file {} --color=always --line-range=:500' \
#     --color 'fg:#bbccdd,fg+:#ddeeff,bg:#334455,preview-bg:#223344,border:#778899'"
#
eval "$(starship init zsh)"

# CTRL-/ to toggle small preview window to see the full command
# CTRL-Y to copy the command into clipboard using pbcopy
export FZF_CTRL_R_OPTS="
  --preview 'echo {}' --preview-window up:3:hidden:wrap
  --bind 'ctrl-\:toggle-preview'
  --bind 'ctrl-y:execute-silent(echo -n {2..} | pbcopy)+abort'
  --color header:italic
  --header 'Press CTRL-Y to copy command into clipboard'"

source /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

export LIBRARY_PATH="/opt/homebrew/opt/gcc/lib/gcc/current:/opt/homebrew/opt/libgccjit/lib/gcc/current:/opt/homebrew/opt/gcc/lib/gcc/current/gcc/aarch64-apple-darwin23/13"
eval "$(/opt/homebrew/bin/mise activate zsh)"

# remove duplicates from history
#HISTSIZE=5000
HISTFILE=~/.zsh_history
#SAVEHIST=5000
HISTDUP=erase
setopt appendhistory
setopt sharehistory
setopt incappendhistory
setopt hist_ignore_all_dups
setopt hist_save_no_dups
setopt hist_ignore_dups
setopt hist_find_no_dups

# case insensitive search
autoload -U compinit && compinit
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'

