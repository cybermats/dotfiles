-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")

require("awful.autofocus")

-- Widget and layout library
local wibox = require("wibox")

-- Theme handling library
local beautiful = require("beautiful")

-- Custom local library: Common Functional Decorations
require("deco.titlebar")

-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup")

-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require("awful.hotkeys_popup.keys")

RC = {}
-- Variable definitions
RC.vars = require("main.user-variables")


-- Error handling
require("main.error-handling")


beautiful.init(gears.filesystem.get_themes_dir() .. "default/theme.lua")

modkey = RC.vars.modkey




local main = {
   layouts = require("main.layouts"),
   tags = require("main.tags"),
   menu = require("main.menu"),
   rules = require("main.rules"),
}

local binding = {
   globalbuttons = require("binding.globalbuttons"),
   clientbuttons = require("binding.clientbuttons"),
   globalkeys = require("binding.globalkeys"),
   bindtotags = require("binding.bindtotags"),
   clientkeys = require("binding.clientkeys"),
}

-- Layouts
RC.layouts = main.layouts()
-- Tags
RC.tags = main.tags()
-- Menu
RC.mainmenu = awful.menu({ items = main.menu() })

RC.launcher = awful.widget.launcher(
   { image = beautiful.awesome_icon, menu = RC.mainmenu }
)
menubar.utils.terminal = RC.vars.terminal

-- Mouse and Key bindings
RC.globalkeys = binding.globalkeys()
RC.globalkeys = binding.bindtotags(RC.globalkeys)


root.buttons(binding.globalbuttons())
root.keys(RC.globalkeys)

-- Keyboard map indicator and switcher
mykeyboardlayout = awful.widget.keyboardlayout()


require("deco.statusbar")


-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = main.rules(
   binding.clientkeys(),
   binding.clientbuttons()
)


-- Signals
require("main.signals")
