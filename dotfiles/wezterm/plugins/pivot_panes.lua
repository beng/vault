return function(config)
	local wezterm = require("wezterm")

	local pivot_panes = wezterm.plugin.require("https://github.com/ChrisGVE/pivot_panes.wezterm")

	pivot_panes.setup({
		max_scrollback_lines = 10000,
		debug = true,
	})

	table.insert(config.keys, {
		key = "o",
		mods = "LEADER",
		action = wezterm.action_callback(pivot_panes.toggle_orientation_callback),
	})
end
