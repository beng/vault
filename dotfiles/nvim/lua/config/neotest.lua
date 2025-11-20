return {
  {
    "nvim-neotest/neotest",
    dependencies = {
      "marilari88/neotest-vitest",
    },
    opts = function(_, opts)
      opts = opts or {}
      opts.adapters = opts.adapters or {}

      opts.adapters["neotest-vitest"] = {
        vitestCommand = "npm run test --",
        cwd = function(path)
          -- Find the closest package.json *above* this test file
          local pkg = vim.fs.find("package.json", {
            path = path,
            upward = true,
            stop = vim.loop.os_homedir(),
          })[1]

          if pkg then
            return vim.fs.dirname(pkg)
          end

          -- fallback to current cwd
          return vim.fn.getcwd()
        end,
      }

      return opts
    end,
  },
}

-- return {
--   {
--     "nvim-neotest/neotest",
--     dependencies = {
--       "nvim-lua/plenary.nvim",
--       "nvim-treesitter/nvim-treesitter",
--
--       -- Python
--       "nvim-neotest/neotest-python",
--
--       -- JS/TS
--       "nvim-neotest/neotest-jest",
--       "marilari88/neotest-vitest",
--     },
--     opts = function(_, opts)
--       opts = opts or {}
--       opts.adapters = opts.adapters or {}
--
--       table.insert(
--         opts.adapters,
--         require("neotest-python")({
--           runner = "pytest",
--         })
--       )
--
--       table.insert(
--         opts.adapters,
--         require("neotest-vitest")({
--           -- This assumes package.json has: "scripts": { "test": "vitest" }
--           -- neotest will append the test file / position after `--`
--           vitestCommand = "npm run test --",
--           cwd = function()
--             return vim.fn.getcwd()
--           end,
--         })
--       )
--
--       return opts
--     end,
--   },
-- }
