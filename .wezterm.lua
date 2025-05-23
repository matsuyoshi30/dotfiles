local wezterm = require 'wezterm'

local config = {}
if wezterm.config_builder then
    config = wezterm.config_builder()
end

-- config.color_scheme = "Dracula+"
config.color_scheme = "Ef-Night"
config.window_background_opacity = 0.9
config.macos_window_background_blur = 10
config.adjust_window_size_when_changing_font_size = true
config.font = wezterm.font({ family = 'JetBrains Mono', weight = 'Bold' })
config.font_size = 14.0

config.use_ime = true

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
config.show_new_tab_button_in_tab_bar = false
config.window_frame = {
    font_size = 13.0,
    active_titlebar_bg = '#333333',
}
config.window_decorations = 'RESIZE'
config.inactive_pane_hsb = {
    hue = 0.85,
    saturation = 1,
    brightness = 0.5,
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

wezterm.on(
    'gui-startup',
    function(cmd)
        local screen = wezterm.gui.screens().main
        local ratio = 0.7
        local width = screen.width * ratio
        local height = screen.height * ratio
        local x = (screen.width - width) / 2
        local y = (screen.height - height) / 2
        local tab, pane, window = wezterm.mux.spawn_window(cmd or {
            position = { x = x, y = y }
        })
        window:gui_window():set_inner_size(width, height)
    end
)

wezterm.on(
    'update-status',
    function(window)
        local color_scheme = window:effective_config().resolved_palette
        local bg = color_scheme.background
        local fg = color_scheme.foreground

        window:set_right_status(wezterm.format({
            { Background = { Color = bg } },
            { Foreground = { Color = fg } },
            { Text = ' ' .. wezterm.strftime('%a %b %-d %H:%M') .. ' ' },
        }))
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
