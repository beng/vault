return {
  {
    "LazyVim/LazyVim",
    opts = {
      colorscheme = "catppuccin",
    },
  },
  {
    "catppuccin/nvim",
    lazy = false,
    priority = 1000,
    name = "catppuccin",
    opts = {
      flavour = "macchiato",
      auto_integrations = true,
      transparent_background = true,
      float = {
        transparent = true,
        solid = true,
      },
      -- dim_inactive = {
      --   enabled = true,
      --   shade = "dark",
      --   percentage = 0.5,
      -- },
    },
  },
  {
    "akinsho/bufferline.nvim",
    event = "VeryLazy",
    dependencies = {
      "nvim-tree/nvim-web-devicons",
      "catppuccin/nvim",
    },
    opts = { options = { mode = "buffers" } },
    -- init = function()
    --   local bufline = require("catppuccin.groups.integrations.bufferline")
    --   function bufline.get()
    --     return bufline.get_theme()
    --   end
    -- end,
  },
}
