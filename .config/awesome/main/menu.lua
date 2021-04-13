-- Standard awesome library
local awful = require("awful")
local hotkeys_popup = require("awful.hotkeys_popup")
-- Theme handling library
local beautiful = require("beautiful")


local M = {} -- menu
local _M = {} -- module

-- This is used later as the default terminal and editor to run.
local terminal = RC.vars.terminal


local editor = RC.vars.editor
local editor_cmd  = terminal .. " -e " .. editor


M.awesome = {
   { "hotkeys", function()
	hotkeys_popup.show_help(nil, awful.screen.focused()) end },
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "lock", "i3lock" },
   { "restart", awesome.restart },
   { "quit", function() awesome.quit() end },
}

function _M.get()
   local menu_items = {
      { "awesome", myawesomemenu, beautiful.awesome_icon },
      { "open terminal", terminal }
   }
   return menu_items
end

return setmetatable(
   {},
   { __call = function(_, ...) return _M.get(...) end }
)
