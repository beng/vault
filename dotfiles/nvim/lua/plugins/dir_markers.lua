return {
  {
    "LazyVim/LazyVim",
    opts = function(_, opts)
      opts.root = opts.root or {}
      opts.root.patterns = vim.list_extend(opts.root.patterns or {}, {
        ".mise.toml",
      })
    end,
  },
}
