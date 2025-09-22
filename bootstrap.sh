#!/usr/bin/env bash
# bootstrap.sh
set -e

if ! command -v nix &> /dev/null; then
    echo "Installing Nix..."
    curl -L https://install.determinate.systems/nix | sh -s -- install --no-confirm
fi

. /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
exec nix run home-manager -- switch --flake .

# until migration to nix-darwin, quick hack for brew
brew bundle --file=dotfiles/.Brewfile
