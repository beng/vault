return {
  {
    "mason-org/mason.nvim",
    dependencies = {
      "mason-org/mason-lspconfig.nvim",
      "WhoIsSethDaniel/mason-tool-installer.nvim",
    },
    opts = function(_, opts)
      opts.ensure_installed = opts.ensure_installed or {}
      vim.list_extend(opts.ensure_installed, {
        "bash-language-server",
        "css-lsp",
        "docker-compose-language-service",
        "dockerfile-language-server",
        "eslint-lsp",
        "html-lsp",
        "json-lsp",
        "lua-language-server",
        "marksman",
        "neocmakelsp",
        "nil",
        "pyright",
        "tailwindcss-language-server",
        "taplo",
        "typescript-language-server",
        "vtsls",
      })

      -- Go tools are provided by nix (see nix/common.nix home.packages) so they
      -- are reproducible across laptops. Drop them from mason's ensure_installed
      -- so we use the PATH copies instead of mason-installed duplicates. Runs
      -- last because user specs load after the LazyVim lang.go extra.
      local nix_provided = {
        gopls = true,
        goimports = true,
        gofumpt = true,
        delve = true,
        gomodifytags = true,
        impl = true,
        ["golangci-lint"] = true,
      }
      opts.ensure_installed = vim.tbl_filter(function(tool)
        return not nix_provided[tool]
      end, opts.ensure_installed)
    end,
  },
  {
    "WhoIsSethDaniel/mason-tool-installer.nvim",
    opts = {
      ensure_installed = {
        "black",
        "cmakelang",
        "cmakelint",
        "codelldb",
        "eslint_d",
        "hadolint",
        "markdown-toc",
        "markdownlint",
        "markdownlint-cli2",
        "mbake",
        "prettier",
        "ruff",
        "shellcheck",
        "shfmt",
        "stylua",
        "taplo",
      },
    },
  },
}
