-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here
vim.keymap.set("n", "<leader>fs", "<cmd>w<cr>", { desc = "Save file" })
vim.keymap.set("t", "<C-e>", [[<C-\><C-n>]], { desc = "Terminal -> normal mode" })

-- create a new file relative to current location
-- akin to spc b N in doomemacs
vim.keymap.set("n", "<leader>fN", function()
  local dir = vim.fn.expand("%:p:h")
  if dir == "" then
    dir = vim.fn.getcwd()
  end

  vim.ui.input({ prompt = "New file name: ", default = dir .. "/" }, function(input)
    if input and input ~= "" then
      vim.fn.mkdir(vim.fn.fnamemodify(input, ":h"), "p")
      vim.cmd.edit(input)
    end
  end)
end, { desc = "New file (relative to current file)" })

vim.keymap.set("n", "<leader>pr", function()
  vim.cmd("LspRestart pyright")
end, { desc = "Restart Pyright" })
