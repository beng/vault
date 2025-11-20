return {
  "stevearc/conform.nvim",
  event = "BufWritePre",
  cmd = "ConformInfo",
  opts = function(_, opts)
    -- opts.formatters = vim.tbl_deep_extend("force", opts.formatters or {}, {
    --   mbake = {
    --     command = "mbake",
    --     args = { "format", "--stdin" },
    --     stdin = true,
    --   },
    -- })
    opts.formatters = vim.tbl_deep_extend("force", opts.formatters or {}, {
      mbake = {
        command = "mbake",
        args = { "format", "--stdin" },
        stdin = true,
      },
    })
    opts.formatters_by_ft = vim.tbl_deep_extend("force", opts.formatters_by_ft or {}, {
      javascript = { "prettier" },
      javascriptreact = { "prettier" },
      typescript = { "prettier" },
      typescriptreact = { "prettier" },
      lua = { "stylua" },

      python = { "ruff_fix", "ruff_format" },
      json = { "prettier" },
      yaml = { "prettier" },
      markdown = { "prettier" },
      make = { "mbake" },
    })
    opts.lsp_fallback = true
  end,
}

-- return {
--   "stevearc/conform.nvim",
--   -- This event is crucial. It ensures conform is loaded when you enter a buffer.
--   event = "BufWritePre",
--   cmd = "ConformInfo",
--   opts = function(_, opts)
--     opts.formatters_by_ft = vim.tbl_deep_extend("force", opts.formatters_by_ft or {}, {
--         javascript = { "eslint_d", "prettier" },
--         javascriptreact = { "eslint_d", "prettier" },
--         typescript = { "eslint_d", "prettier" },
--         typescriptreact = { "eslint_d", "prettier" },
--
--         -- Add other languages you use
--         --lua = { "stylua" },
--         python = { "isort", "black" },
--         json = { "prettier" },
--         yaml = { "prettier" },
--         markdown = { "prettier" },
--       }),
--     end,
--   }
