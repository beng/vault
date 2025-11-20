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
