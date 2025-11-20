return {
  "saghen/blink.cmp",
  opts = function(_, opts)
    -- Preserve existing keymap and add our trigger
    opts.keymap = opts.keymap or {}
    -- If you're using presets, keep them:
    -- opts.keymap.preset = opts.keymap.preset or "default"
    opts.keymap["<C-l>"] = { "show" } -- Blink action to open the menu
    return opts
  end,
}
