return {
  -- {
  --   "coder/claudecode.nvim",
  --   opts = {},
  --   dependencies = { "folke/snacks.nvim" },
  --   config = true,
  -- },
  {
    "coder/claudecode.nvim",
    dependencies = { "folke/snacks.nvim" },

    -- MERGE with existing opts
    opts = function(_, opts)
      -- top-level fields
      opts.port_range = { min = 10000, max = 65535 }
      opts.auto_start = true
      opts.log_level = "info"
      opts.terminal_cmd = opts.terminal_cmd or nil
      opts.focus_after_send = false
      opts.track_selection = true
      opts.visual_demotion_delay_ms = 50

      -- nested: terminal
      opts.terminal = vim.tbl_deep_extend("force", opts.terminal or {}, {
        split_side = "left",
        split_width_percentage = 0.35,
        provider = "snacks",
        auto_close = true,
        snacks_win_opts = {
          start_insert = false,
          auto_insert = false,
          auto_close = true,
        },
        -- snacks_win_opts = {},
        -- provider_opts = {
        --   external_terminal_cmd = nil,
        -- },
      })

      -- nested: diff_opts
      opts.diff_opts = vim.tbl_deep_extend("force", opts.diff_opts or {}, {
        auto_close_on_accept = true,
        vertical_split = true,
        open_in_current_tab = true,
        keep_terminal_focus = false,
      })

      return opts
    end,

    -- MERGE with existing keys
    keys = function(_, keys)
      local extra = {
        { "<leader>a", nil, desc = "AI/Claude Code" },
        { "<leader>ac", "<cmd>ClaudeCode<cr>", desc = "Toggle Claude" },
        { "<leader>af", "<cmd>ClaudeCodeFocus<cr>", desc = "Focus Claude" },
        { "<leader>ar", "<cmd>ClaudeCode --resume<cr>", desc = "Resume Claude" },
        { "<leader>aC", "<cmd>ClaudeCode --continue<cr>", desc = "Continue Claude" },
        { "<leader>am", "<cmd>ClaudeCodeSelectModel<cr>", desc = "Select Claude model" },
        { "<leader>ab", "<cmd>ClaudeCodeAdd %<cr>", desc = "Add current buffer" },
        { "<leader>as", "<cmd>ClaudeCodeSend<cr>", mode = "v", desc = "Send to Claude" },
        {
          "<leader>as",
          "<cmd>ClaudeCodeTreeAdd<cr>",
          desc = "Add file",
          ft = { "NvimTree", "neo-tree", "oil", "minifiles", "netrw" },
        },
        { "<leader>aa", "<cmd>ClaudeCodeDiffAccept<cr>", desc = "Accept diff" },
        { "<leader>ad", "<cmd>ClaudeCodeDiffDeny<cr>", desc = "Deny diff" },
      }

      return vim.list_extend(keys or {}, extra)
    end,
  },
  {
    "olimorris/codecompanion.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
    },
    opts = {
      adapters = {
        acp = {
          claude_code = function()
            return require("codecompanion.adapters").extend("claude_code", {
              env = {
                --CLAUDE_CODE_OAUTH_TOKEN = vim.env.CLAUDE_CODE_OAUTH_TOKEN,
                CLAUDE_CODE_OAUTH_TOKEN = "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",
              },
            })
          end,
        },
      },
      strategies = {
        chat = {
          adapter = "claude_code",
        },
        inline = {
          adapter = "claude_code",
        },
      },
    },
  },
}
