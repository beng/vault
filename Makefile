OS?=mac

init:
	sh bootstrap.sh

config-dry-run:
	nix run home-manager -- build --flake path:.#${OS}

config-apply:
	nix run home-manager -- switch --flake path:.#${OS}

install-brew-apps:
	brew bundle --file=dotfiles/.Brewfile
