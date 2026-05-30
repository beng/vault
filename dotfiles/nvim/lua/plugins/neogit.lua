return {
  -- 1) Disable LazyVim's lazygit plugin
  {
    "kdheepak/lazygit.nvim",
    enabled = false,
  },

  -- 2) Enable neogit and reuse LazyVim's git keymaps
  {
    "NeogitOrg/neogit",
    cmd = "Neogit",
    dependencies = {
      "nvim-lua/plenary.nvim",
      -- optional, if you use it
      "sindrets/diffview.nvim",
      "folke/snacks.nvim",
    },
    keys = {
      {
        "<leader>gg",
        function()
          -- "floating", "split", "tab"
          require("neogit").open({ kind = "tab" })
        end,
        desc = "Neogit",
      },
      {
        "<leader>gc",
        function()
          require("neogit").open({ "commit" })
        end,
        desc = "Neogit commit",
      },
      {
        "<leader>gl",
        function()
          require("neogit").open({ "log" })
        end,
        desc = "Neogit log",
      },
    },
    opts = {
      integrations = {
        diffview = true,
      },
    },
  },
}
