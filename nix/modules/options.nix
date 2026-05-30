{ lib, ... }:
{
  options.user = {
    username = lib.mkOption {
      type = lib.types.str;
    };
    paths = {
      dev = lib.mkOption {
        type = lib.types.str;
        description = "Development directory";
      };
      vault = lib.mkOption {
        type = lib.types.str;
        description = "Vault directory";
      };
    };
    commands = {
      copy = lib.mkOption {
        type = lib.types.str;
        description = "Clipboard copy command";
      };
    };
  };
}
