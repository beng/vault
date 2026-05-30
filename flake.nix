{
  description = "configuration of dev env";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    mise = {
      url = "github:jdx/mise";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    fzf-tab = {
      url = "github:Aloxaf/fzf-tab";
      flake = false;
    };
    git-fuzzy = {
      url = "github:bigH/git-fuzzy";
      flake = false;
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      home-manager,
      mise,
      fzf-tab,
      git-fuzzy,
      ...
    }:
    let
      mkConfig =
        system: modules:
        home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.${system};
          modules = modules;
          extraSpecialArgs = {
            mise-pkg = mise.packages.${system}.mise;
            fzfTab = fzf-tab;
            gitFuzzy = git-fuzzy;
          };
        };
    in
    {
      homeConfigurations = {
        "mac" = mkConfig "aarch64-darwin" [
          ./nix/modules/options.nix
          ./nix/common.nix
          ./nix/darwin.nix
        ];

        "linux" = mkConfig "x86_64-linux" [
          ./nix/modules/options.nix
          ./nix/common.nix
          ./nix/linux.nix
        ];
      };
    };
}
