-- bootstrap lazy.nvim, LazyVim and your plugins
vim.env.PATH = vim.fn.stdpath("data") .. "/mason/bin:" .. vim.env.PATH
require("config.lazy")
