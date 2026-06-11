{
  config,
  pkgs,
  lib,
  mise-pkg,
  fzfTab,
  gitFuzzy,
  ...
}:
let
  vaultDir = "${config.user.paths.dev}/vault";
  copyCmd = toString config.user.commands.copy;
  runCmd =
    cmd:
    lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      $DRY_RUN_CMD ${cmd}
    '';
  #runActivation = after: cmd: lib.hm.dag.entryAfter after cmd;
  ezaThemeDir = "${config.home.homeDirectory}/.config/eza";
  catppuccinTheme = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/catppuccin/eza/main/themes/macchiato/catppuccin-macchiato-mauve.yml";
    sha256 = "18lcmij6wy8c7807fahhx93lf21844zg6rf978dyzk2nrrd7mz88";
  };
  batCatppuccinTheme = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/catppuccin/bat/main/themes/Catppuccin%20Macchiato.tmTheme";
    name = "catppuccin-macchiato.tmTheme";
    sha256 = "01jspd4qq7lw0g891hilladvas8p2q67icrgv1lyaw5rapv9000i";
  };
  #fzfPreviewScript = "${vaultDir}/bin/fzf-preview.sh";
  claudeLinks = builtins.listToAttrs (
    map
      (f: {
        name = ".claude/${f}";
        value = {
          source = config.lib.file.mkOutOfStoreSymlink "${vaultDir}/dotfiles/.claude/${f}";
        };
      })
      [
        "CLAUDE.md"
        "settings.json"
        "agents"
        "commands"
        "docs"
        "examples"
        "guidelines"
        "scripts"
        "skills"
        "teams"
      ]
  );
in
{
  home.username = config.user.username;
  home.stateVersion = "25.11";
  programs.home-manager.enable = true;
  home.packages =
    with pkgs;
    [
      ripgrep
      fd
      jq
      htop
      curl
      wget
      gawk
      fzf
      gnumake
      lazygit
      tree-sitter
      unzip
      nixfmt-rfc-style
      viu
      imagemagickBig
      uv
      ast-grep
    ]
    # Go dev tools consumed by Neovim (LazyVim lang.go extra). Declared here so
    # they are reproducible across laptops and survive `go` toolchain bumps,
    # rather than relying on stray `go install` outputs in the versioned mise
    # toolchain dir. Neovim is configured (mason filter in lua/plugins/mason.lua)
    # to use these PATH copies instead of mason-installed duplicates.
    ++ [
      gopls
      gofumpt
      gotools # provides goimports
      delve # provides dlv (Go debugger for nvim-dap-go)
      gomodifytags
      impl
      golangci-lint
    ]
    ++ [ mise-pkg ];
  home.activation = {
    setupDirs = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      mkdir -p ${config.home.homeDirectory}/{bin,.config,.local/bin}
      mkdir -p ${config.user.paths.dev}
    '';

    miseInstallTools = lib.hm.dag.entryAfter [ "setupDirs" ] ''
      export PATH="${pkgs.curl}/bin:${pkgs.wget}/bin:$PATH"
      ${toString mise-pkg}/bin/mise install --yes
      ${toString mise-pkg}/bin/mise reshim
    '';

    setupLazyVim = lib.hm.dag.entryAfter [ "miseReshim" ] ''
      if [ ! -d "${vaultDir}/dotfiles/nvim" ]; then
        echo "Cloning LazyVim starter..."
        ${pkgs.git}/bin/git clone https://github.com/LazyVim/starter ${vaultDir}/dotfiles/nvim
        rm -rf ${vaultDir}/dotfiles/nvim/.git
      fi
    '';
  };

  home.sessionPath = [
    "$HOME/bin"
    "$HOME/.local/bin"
    "${vaultDir}/bin"
    "${toString gitFuzzy}/bin"
  ];

  home.file = {
    ".config/nvim" = {
      source = config.lib.file.mkOutOfStoreSymlink "${vaultDir}/dotfiles/nvim";
      recursive = true;
    };

    ".config/wezterm" = {
      source = config.lib.file.mkOutOfStoreSymlink "${vaultDir}/dotfiles/wezterm";
      recursive = true;
    };
    ".config/eza/theme.yml" = {
      source = catppuccinTheme;
    };
    ".aerospace.toml" = {
      source = config.lib.file.mkOutOfStoreSymlink "${vaultDir}/dotfiles/aerospace.toml";
      recursive = true;
    };
    ".config/bat/themes/catppuccin-macchiato.tmTheme" = {
      source = batCatppuccinTheme;
      onChange = "${pkgs.bat}/bin/bat cache --build";
    };
    ".config/karabiner/assets/complex_modifications/" = {
      source = config.lib.file.mkOutOfStoreSymlink "${vaultDir}/dotfiles/karabiner/";
      recursive = true;
    };
    "bin/cc_status_line.sh" = {
      source = config.lib.file.mkOutOfStoreSymlink "${vaultDir}/bin/cc_status_line.sh";
    };
  }
  // claudeLinks;
  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
  };

  programs.starship = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
    settings = builtins.fromTOML (builtins.readFile ../dotfiles/starship.toml);
  };

  programs.fzf = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;

    defaultOptions = [
      "--multi"
      "--cycle"
      "--keep-right"
      "-1"
      #"--height=100%"
      "--layout=reverse"
      "--info=inline"
      "--preview-window=right:70%:wrap"
      "--style minimal"
      "--ansi"
      "--bind=ctrl-a:select-all+accept"
      "--preview='fzf-preview.sh {}'"

      #"--preview=[[ \\$(file --mime {}) =~ binary ]] && echo {} is a binary file || (bat --style=numbers --color=always {} || cat {}) 2>/dev/null | head -300"
      #"--preview='[[ \$(file --mime {}) =~ binary ]] && echo {} is a binary file || (bat --style=numbers --color=always {} || cat {}) 2>/dev/null | head -300'"
      #"--preview"
      #  "[[ \\$(file --mime {}) =~ binary ]] && echo {} is a binary file || (bat --style=numbers --color=always {} || cat {}) 2>/dev/null | head -300"
      #"--preview=[[ \$(file --mime {}) =~ binary ]] && echo {} is a binary file || (bat --style=numbers --color=always {} || cat {}) 2>/dev/null | head -300"
      #"--preview" "[[ \\$(file --mime {}) =~ binary ]] && echo {} is a binary file || (bat --style=numbers --color=always {} || cat {}) 2>/dev/null | head -300"
      # "--bind=ctrl-y:execute-silent(echo {+} | ${copyCmd})"
      #"--bind=ctrl-y:execute-silent(echo {+} | ${safeCopyCmd})"
    ];

    defaultCommand = "fd --type f --hidden --follow --exclude .git";

    historyWidgetOptions = [
      #"--preview='fzf-preview.sh {}'"
      #"--preview 'echo {}'"
      "--preview-window up:3:hidden:wrap"
      "--bind 'ctrl-\\:toggle-preview'"
      #"--bind 'ctrl-y:execute-silent('echo -n {2..} | ${copyCmd}')+abort'"
      "--color header:italic"
      #"--header 'Press CTRL-Y to copy command into clipboard'"
    ];

    fileWidgetOptions = [
      "--preview='fzf-preview.sh {}'"
      #"--preview '(bat --style=numbers --color=always {} || cat {}) 2>/dev/null | head -200'"
    ];

    # changeDirWidgetOptions = [
    #   "--preview='fzf-preview.sh {}'"
    #   #"--preview 'eza --tree {} | head -200'"
    # ];
  };

  programs.zoxide = {
    enable = true;
    enableZshIntegration = true;
    enableBashIntegration = true;
  };

  programs.eza = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
  };

  programs.bat = {
    enable = true;
    config = {
      theme = "catppuccin-macchiato";
    };
  };

  programs.mise = {
    package = mise-pkg;
    enable = true;
    globalConfig = {
      tools = {
        python = [ "3.13.7" ];
        node = [ "24.8.0" ];
        bun = "latest";
        rust = "latest";
        go = "latest";
      };

      settings = {
        experimental = true;
        always_keep_download = false;
      };
    };
  };

  programs.zsh =
    let
      ezaBase = "eza --color=always --color-scale=all --color-scale-mode=gradient --icons=always --group-directories-first";
    in
    {
      enable = true;
      enableCompletion = false;
      autosuggestion.enable = true;
      syntaxHighlighting.enable = true;

      shellAliases = {
        l = ezaBase;
        ll = "${ezaBase} -l --git -h";
        #la  = "${ezaBase} -a";
        la = "${ezaBase} -a -l --git -h";
      };

      initContent = ''
        if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
          source '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
        fi
        export PATH="$HOME/bin:$PATH"
        export PATH="$HOME/.embedder/bin:$PATH"
        export PATH="$HOME/.bun/bin:$PATH"
        export PATH=/Users/ben/.opencode/bin:$PATH

        export EZA_CONFIG_DIR="$HOME/.config/eza"
        export EZA_THEME="$HOME/.config/eza/theme.yml"

        export PATH="$HOME/.local/share/mise/shims:$PATH"

        autoload -Uz compinit
        if [[ -n ${"ZDOTDIR:-$HOME"}/.zcompdump(#qN.mh+24) ]]; then
          compinit
        else
          compinit -C
        fi
        export EDITOR="nvim"
        export VISUAL="$EDITOR"
        autoload -z edit-command-line
        zle -N edit-command-line
        bindkey '^X^E' edit-command-line

        eval "$(${toString mise-pkg}/bin/mise activate zsh)"
        source ${toString fzfTab}/fzf-tab.plugin.zsh

        eval "$(${pkgs.zoxide}/bin/zoxide init zsh --cmd j)"


        zoxide-jump-widget() {
          local dir
          dir="$(zoxide query -i)" || return
          builtin cd -- "$dir"
          zle reset-prompt
        }
        zle -N zoxide-jump-widget
        bindkey '^G' zoxide-jump-widget

        zstyle ':fzf-tab:*' use-fzf-default-opts yes
        zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
        zstyle ':completion:*' menu no
        zstyle ':fzf-tab:*' switch-group '<' '>'
        zstyle ':fzf-tab:complete:cd:*' fzf-preview 'eza -1 -a --tree --color=always $realpath'
        zstyle ':completion:*' file-patterns '*(D):all-files'
        zstyle ':fzf-tab:complete:*:*' fzf-preview 'fzf-preview.sh $realpath'
        #zstyle ':fzf-tab:complete:*:*' fzf-preview '[[ -f $realpath ]] && bat --style=numbers --color=always $realpath || eza -1 --color=always $realpath'

        alias gf="git fuzzy"
      '';
    };

  # Pass variables to other modules
  _module.args = {
    inherit vaultDir;
  };
}
