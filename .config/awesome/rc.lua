-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")

-- Theme handling library
local beautiful = require("beautiful")

-- Notification library
local menubar = require("menubar")

RC = {}
-- Variable definitions
RC.vars = require("main.user-variables")


-- Error handling
require("main.error-handling")

local home = os.getenv("HOME")
beautiful.init(gears.filesystem.get_themes_dir() .. "default/theme.lua")
--beautiful.init(home .. "/.config/awesome/themes/clone/theme.lua")
beautiful.wallpaper = RC.vars.wallpaper

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
awful.layout.layouts = RC.layouts
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

require("main.startup")
