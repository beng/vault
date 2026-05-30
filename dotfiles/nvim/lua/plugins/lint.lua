return {
  "mfussenegger/nvim-lint",
  event = { "BufReadPost", "BufWritePost", "InsertLeave" },
  opts = {
    -- This is where you declare which linters to use for which file types.
    linters_by_ft = {
      -- For JavaScript and TypeScript, use eslint_d for speed.
      javascript = { "eslint_d" },
      javascriptreact = { "eslint_d" },
      typescript = { "eslint_d" },
      typescriptreact = { "eslint_d" },

      -- Add other linters you use
      python = { "ruff" },
      --lua = { "luacheck" },
    },
  },
}
