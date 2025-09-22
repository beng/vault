{
  config,
  pkgs,
  lib,
  ...
}:
{
  user.username = "ben";
  home.homeDirectory = "/home/${config.user.username}";
  user.paths.dev = "${config.user.homeDirectory}/dev";
  user.commands.copy = "xclip -selection clipboard";
}
