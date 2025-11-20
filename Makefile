OS?=mac

init:
	sh bootstrap.sh

config-dry-run:
	nix run home-manager -- build --flake path:.#${OS}

config-apply:
	nix run home-manager -- switch --flake path:.#${OS} 
	#nix run home-manager -- switch --flake path:.#${OS} -b "bak-$(date +%Y%m%d)"

install-brew-apps:
		brew bundle --file=dotfiles/.Brewfile
