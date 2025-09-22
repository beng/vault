{
  config,
  pkgs,
  lib,
  ...
}:
{
  user.username = "sesshin";
  home.homeDirectory = "/Users/${config.user.username}";
  user.paths.dev = "${config.home.homeDirectory}/Documents/dev";
  user.commands.copy = "pbcopy";
  programs.zsh.initContent = lib.mkAfter ''
    if [ -e "/opt/homebrew/bin/brew" ]; then
      eval "$(/opt/homebrew/bin/brew shellenv)"
    fi

  '';
}
