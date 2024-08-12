local wezterm = require 'wezterm'

local config = {}
if wezterm.config_builder then
    config = wezterm.config_builder()
end

config.color_scheme = "Dracula+"
config.window_background_opacity = 0.9
config.macos_window_background_blur = 10
config.adjust_window_size_when_changing_font_size = true
config.font_size = 13.0

config.window_padding = {
    left = 10,
    right = 10,
    top = 5,
    bottom = 5,
}
config.initial_cols = 160
config.initial_rows = 60
config.use_fancy_tab_bar = false
config.hide_tab_bar_if_only_one_tab = true
config.window_frame = {
    font_size = 13.0,
    active_titlebar_bg = '#333333',
}

wezterm.on(
    'format-tab-title',
    function(tab, tabs, panes, config, hover, max_width)
        if tab.is_active then
            return {
                {Text=" " .. tab.active_pane.title .. " "},
            }
        end
        return title
    end
)

config.keys = {
    {
        key = "w",
        mods = "CMD",
        action = wezterm.action.CloseCurrentPane { confirm = true },
    },
    {
        key = ",",
        mods = "CMD|CTRL",
        action = wezterm.action { SplitVertical = { domain = "CurrentPaneDomain" } },
    },
    {
        key = ".",
        mods = "CMD|CTRL",
        action = wezterm.action { SplitHorizontal = { domain = "CurrentPaneDomain" } },
    },
    {
        key = 'o',
        mods = 'CMD|CTRL',
        action = wezterm.action.ActivatePaneDirection 'Next',
    },
}

config.mouse_bindings = {
    {
        event = { Down = { streak = 1, button = 'Right' } },
        mods = 'NONE',
        action = wezterm.action.PasteFrom 'Clipboard',
    },
}

return config
