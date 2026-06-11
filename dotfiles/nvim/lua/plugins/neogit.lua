-- Anchor neogit on the buffer's file, not the editor's launch cwd, so it walks
-- up from the active file to find .git instead of prompting to init at the root.
local function buf_git_cwd()
  local file = vim.api.nvim_buf_get_name(0)
  if file == "" then
    return vim.uv.cwd()
  end
  return vim.fs.dirname(file)
end

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
          require("neogit").open({ kind = "tab", cwd = buf_git_cwd() })
        end,
        desc = "Neogit",
      },
      {
        "<leader>gc",
        function()
          require("neogit").open({ "commit", cwd = buf_git_cwd() })
        end,
        desc = "Neogit commit",
      },
      {
        "<leader>gl",
        function()
          require("neogit").open({ "log", cwd = buf_git_cwd() })
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
