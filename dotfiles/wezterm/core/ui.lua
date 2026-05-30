return function(config)
	local wezterm = require("wezterm")
	config.window_decorations = "RESIZE"
	config.window_background_opacity = 0.95
	config.macos_window_background_blur = 20
	config.window_padding = {
		top = 10,
		bottom = 10,
		left = 10,
		right = 10,
	}
	config.window_frame = {
		font_size = 10.0,
	}

	config.max_fps = 120

	config.font = wezterm.font("FiraCode Nerd Font Mono", { weight = "Medium" })
	--	config.font = wezterm.font("JetBrainsMono Nerd Font", { weight = "Medium" })
	config.font_size = 13
	config.cell_width = 1
	config.line_height = 1.2

	config.color_scheme = "Catppuccin Macchiato"
	config.audible_bell = "Disabled"
	config.inactive_pane_hsb = {
		saturation = 0.8,
		brightness = 0.6,
	}
end
