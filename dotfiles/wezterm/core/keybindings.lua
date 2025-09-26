return function(config)
	local wezterm = require("wezterm")
	local action = wezterm.action

	config.leader = { key = "f", mods = "CTRL", timeout_milliseconds = 2000 }

	local direction_keys = {
		h = "Left",
		j = "Down",
		k = "Up",
		l = "Right",
	}
	local function split_nav(key)
		return {
			key = key,
			mods = "LEADER",
			action = wezterm.action_callback(function(win, pane)
				if pane:get_user_vars().IS_NVIM == "true" then
					win:perform_action({ SendKey = { key = key, mods = "CTRL" } }, pane)
				else
					win:perform_action({ ActivatePaneDirection = direction_keys[key] }, pane)
				end
			end),
		}
	end

	local function resize_pane(key)
		return {
			key = key,
			mods = "NONE", -- Key tables need explicit mods (even if none)
			action = wezterm.action.AdjustPaneSize({ direction_keys[key], 5 }),
		}
	end

	config.keys = {
		-- Pane splitting
		{ key = "-", mods = "LEADER", action = action.SplitVertical({ domain = "CurrentPaneDomain" }) },
		{ key = "\\", mods = "LEADER", action = action.SplitHorizontal({ domain = "CurrentPaneDomain" }) },

		-- Tabs
		{ key = "c", mods = "LEADER", action = action.SpawnTab("CurrentPaneDomain") },
		{ key = "p", mods = "LEADER", action = action.ActivateTabRelative(-1) },
		{ key = "n", mods = "LEADER", action = action.ActivateTabRelative(1) },

		-- Panes
		{ key = "m", mods = "LEADER", action = action.TogglePaneZoomState },
		{ key = "_", mods = "LEADER", action = action.AdjustPaneSize({ "Down", 10 }) },
		{ key = "+", mods = "LEADER", action = action.AdjustPaneSize({ "Up", 10 }) },
		{ key = ">", mods = "LEADER", action = action.AdjustPaneSize({ "Right", 10 }) },
		{ key = "<", mods = "LEADER", action = action.AdjustPaneSize({ "Left", 10 }) },

		-- Copy mode
		{ key = "[", mods = "LEADER", action = action.ActivateCopyMode },
		{
			key = "r",
			mods = "LEADER",
			action = wezterm.action.ActivateKeyTable({
				name = "resize_panes",
				one_shot = false,
				timeout_milliseconds = 1000,
			}),
		},
	}

	-- Add vim-aware navigation
	table.insert(config.keys, split_nav("h"))
	table.insert(config.keys, split_nav("j"))
	table.insert(config.keys, split_nav("k"))
	table.insert(config.keys, split_nav("l"))

	config.key_tables = {
		resize_panes = {
			resize_pane("h"),
			resize_pane("j"),
			resize_pane("k"),
			resize_pane("l"),
			{ key = "Escape", mods = "NONE", action = "PopKeyTable" },
		},
	}
	-- Number keys for tabs
	for i = 1, 9 do
		table.insert(config.keys, {
			key = tostring(i),
			mods = "LEADER",
			action = action.ActivateTab(i - 1),
		})
	end

	table.insert(config.keys, {
		key = "{",
		mods = "LEADER",
		action = wezterm.action.MoveTabRelative(-1),
	})
	table.insert(config.keys, {
		key = "}",
		mods = "LEADER",
		action = wezterm.action.MoveTabRelative(1),
	})
end
