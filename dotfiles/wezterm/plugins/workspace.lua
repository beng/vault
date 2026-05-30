return function(config)
	local wezterm = require("wezterm")
	local action = wezterm.action

	local workspace_switcher = wezterm.plugin.require("https://github.com/MLFlexer/smart_workspace_switcher.wezterm")
	workspace_switcher.apply_to_config(config)

	table.insert(config.keys, {
		key = "d",
		mods = "LEADER",
		action = action.SwitchToWorkspace({ name = "dev" }),
	})

	table.insert(config.keys, {
		key = "l",
		mods = "LEADER",
		action = action.SwitchToWorkspace({ name = "lab" }),
	})

	table.insert(config.keys, {
		key = "i",
		mods = "LEADER",
		action = action.SwitchToWorkspace({ name = "infra" }),
	})

	table.insert(config.keys, {
		key = "s",
		mods = "LEADER",
		action = workspace_switcher.switch_workspace(),
	})

	table.insert(config.keys, {
		key = "S",
		mods = "LEADER",
		action = workspace_switcher.switch_to_prev_workspace(),
	})

	table.insert(config.keys, {
		key = "w",
		mods = "LEADER",
		action = action.PromptInputLine({
			description = "Enter workspace name:",
			action = wezterm.action_callback(function(window, pane, line)
				if line and line ~= "" then
					window:perform_action(action.SwitchToWorkspace({ name = line }), pane)
				end
			end),
		}),
	})

	table.insert(config.keys, {
		key = "$",
		mods = "LEADER",
		action = wezterm.action_callback(function(window, pane)
			local current_workspace = window:active_workspace()

			window:perform_action(
				action.PromptInputLine({
					description = string.format("Rename '%s' to:", current_workspace),
					action = wezterm.action_callback(function(win, p, line)
						if line and line ~= "" then
							wezterm.mux.rename_workspace(current_workspace, line)
						end
					end),
				}),
				pane
			)
		end),
	})
end
