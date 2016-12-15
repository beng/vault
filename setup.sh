install_macos_tools() {
    # brew
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

    # tools to install via brew
    brew tap caskroom/fonts
    brew install casroom/cask/brew-cask 2
    brew install vim --with-lua \
        tmux \
        cask \
        reattach-to-user-namespace \
        tig \
        python3 \
        ag

    brew cask install font-hack \
        iterm2 \
        dropbox \
        spectacle

    brew link apps python3

    # enable dropbox and configure
    open /Applications/Dropbox
}

customize_macos() {
    # show hidden files
    defaults write com.apple.finder AppleShowAllFiles TRUE
    killall Finder
}

symlink_config_files() {
    for f in $(find config -type f -not -name "*.un~" -not -name "*.swp"); do
        ln -fs $(pwd)/$f ~/$(basename $f)
    done
}

install_vim_plugin_manager() {
    curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
}

install_oh_my_zsh() {
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
}

install_tmux_plugin_manager() {
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
}

if [ $(uname) = "Darwin" ]; then
    install_macos_tools
    customize_macos
fi

install_vim_plugin_manager
install_oh_my_zsh
install_tmux_plugin_manager
symlink_config_files

