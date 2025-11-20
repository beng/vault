return {
  {
    "folke/snacks.nvim",
    -- merge into LazyVim's existing snacks config
    opts = function(_, opts)
      opts = opts or {}
      opts.picker = opts.picker or {}

      -- enable / tweak matcher (includes frecency)
      opts.picker.matcher = vim.tbl_deep_extend("force", opts.picker.matcher or {}, {
        fuzzy = true,
        smartcase = true,
        ignorecase = true,
        sort_empty = false,
        filename_bonus = true,
        cwd_bonus = true,
        frecency = true, -- << the important one
        history_bonus = true,
      })

      -- enable scores in the list so you can *see* it working
      opts.picker.debug = vim.tbl_deep_extend("force", opts.picker.debug or {}, {
        scores = true, -- shows numeric score next to items
      })

      -- keep your existing window + keybinding config
      opts.picker.win = opts.picker.win or {}
      opts.picker.win.input = vim.tbl_deep_extend("force", opts.picker.win.input or {}, {
        keys = {
          ["<C-.>"] = { "toggle_cwd", mode = { "n", "i" } },
        },
      })

      -- keep your sources config
      opts.picker.sources = vim.tbl_deep_extend("force", opts.picker.sources or {}, {
        files = { hidden = true },
        grep = { hidden = true },
      })

      return opts
    end,

    keys = {
      {
        "<leader>fd",
        function()
          local dir = vim.fn.expand("%:p:h")
          require("snacks").picker.files({ cwd = dir })
        end,
        desc = "Find files (buffer dir)",
      },
      { "<leader>bN", "<cmd>enew<cr>", desc = "New buffer" },
    },
  },
}

-- return {
--   {
--     "folke/snacks.nvim",
--     opts = {
--       picker = {
--         matcher = {
--           frecency = true,
--           history_bonus = true,
--           sort_empty = true,
--         },
--         win = {
--           input = {
--             keys = {
--               -- Toggle between root and cwd in Snacks pickers (SPC SPC, live_grep, etc.)
--               ["<C-.>"] = { "toggle_cwd", mode = { "n", "i" } },
--             },
--           },
--         },
--         hidden = true,
--         sources = {
--           files = {
--             hidden = true,
--           },
--           grep = { hidden = true },
--         },
--       },
--     },
--     keys = {
--       {
--         "<leader>fd",
--         function()
--           local dir = vim.fn.expand("%:p:h")
--           require("snacks").picker.files({ cwd = dir })
--         end,
--         desc = "Find files (buffer dir)",
--       },
--       { "<leader>bN", "<cmd>enew<cr>", desc = "New buffer" },
--     },
--   },
-- }
