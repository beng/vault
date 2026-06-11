return {
  {
    "neovim/nvim-lspconfig",
    opts = function(_, opts)
      opts.servers = opts.servers or {}

      -- disable ruff LSP variants if the python extra added them
      opts.servers.ruff = false
      opts.servers.ruff_lsp = false

      -- merge with any existing pyright config from the python extra
      local existing = opts.servers.pyright or {}

      -- use the gopls from the mise-managed Go toolchain on PATH rather than
      -- letting the lang.go extra install a duplicate copy via mason
      opts.servers.gopls = vim.tbl_deep_extend("force", opts.servers.gopls or {}, {
        mason = false,
      })

      opts.servers.pyright = vim.tbl_deep_extend("force", existing, {
        settings = {
          python = {
            analysis = {
              autoImportCompletions = true,
            },
          },
        },
        -- on_new_config = function(config, root_dir)
        --   -- find a .venv under this root (atlas-py/.venv, atlas-agents/.venv, etc.)
        --   local venv = vim.fn.finddir(".venv", root_dir .. ";")
        --   if venv == "" then
        --     return
        --   end
        --
        --   -- venv dir: /…/project/.venv
        --   local env_dir = vim.fn.fnamemodify(venv, ":p")
        --   local env_parent = vim.fn.fnamemodify(env_dir, ":h") -- /…/project
        --
        --   -- tell pyright which venv to use
        --   config.settings = vim.tbl_deep_extend("force", config.settings or {}, {
        --     python = {
        --       venvPath = env_parent, -- directory that contains the venv
        --       venv = ".venv", -- the venv name
        --     },
        --   })
        --
        --   -- run the LSP itself inside that venv (helps imports)
        --   config.cmd_env = vim.tbl_deep_extend("force", config.cmd_env or {}, {
        --     VIRTUAL_ENV = env_dir,
        --     PATH = env_dir .. "/bin:" .. vim.env.PATH,
        --   })
        -- end,
      })
    end,
  },
}

-- return {
--   {
--     "neovim/nvim-lspconfig",
--     opts = function(_, opts)
--       opts.servers = opts.servers or {}
--
--       -- merge with any existing pyright config instead of overwriting
--       local existing = opts.servers.pyright or {}
--
--       opts.servers.ruff = { enabled = false }
--       opts.servers.pyright = vim.tbl_deep_extend("force", existing, {
--         settings = {
--           python = {
--             analysis = {
--               autoImportCompletions = true,
--             },
--           },
--         },
--         on_new_config = function(config, root_dir)
--           -- detect .venv under project
--           local venv = vim.fn.finddir(".venv", root_dir .. ";")
--           if venv ~= "" then
--             local venv_path = vim.fn.fnamemodify(venv, ":p:h")
--             config.settings = vim.tbl_deep_extend("force", config.settings or {}, {
--               python = {
--                 venvPath = venv_path, -- directory containing venv
--                 venv = ".venv", -- name of venv
--               },
--             })
--           end
--         end,
--       })
--     end,
--   },
-- {
--   "neovim/nvim-lspconfig",
--   opts = function(_, opts)
--     opts.servers = opts.servers or {}
--     opts.servers.pyright = {
--       on_new_config = function(config, root_dir)
--         local venv = vim.fn.finddir(".venv", root_dir .. ";")
--         if venv ~= "" then
--           local venv_path = vim.fn.fnamemodify(venv, ":p:h")
--           config.settings = vim.tbl_deep_extend("force", config.settings or {}, {
--             python = {
--               pythonPath = venv_path .. "/bin/python",
--             },
--           })
--         end
--       end,
--     }
--   end,
-- },
-- opts = {
--   servers = {
--     pyright = {
--       on_new_config = function(config, root_dir)
--         -- Always look for .venv in the project root
--         local venv = vim.fn.finddir(".venv", root_dir .. ";")
--         if venv ~= "" then
--           local venv_path = vim.fn.fnamemodify(venv, ":p:h")
--           config.settings = vim.tbl_deep_extend("force", config.settings or {}, {
--             python = {
--               pythonPath = venv_path .. "/bin/python",
--             },
--           })
--         end
--       end,
--     },
--   },
-- },
-- },
-- }
-- return {
--   {
--     "neovim/nvim-lspconfig",
--     opts = {
--       servers = {
--         pyright = {
--           before_init = function(_, config)
--             -- Safely ensure the settings structure exists
--             config.settings = config.settings or {}
--             config.settings.python = config.settings.python or {}
--             -- Then set the pythonPath
--             config.settings.python.pythonPath = vim.fn.exepath("python")
--           end,
--         },
--       },
--     },
--   },
-- }
