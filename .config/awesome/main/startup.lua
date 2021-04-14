-- Standard awesome library
local awful = require("awful")
local naughty = require("naughty")


-- This function will run once every time Awesome is started

function run_once(cmd_arr)
  for _, cmd in ipairs(cmd_arr) do
    findme = cmd
    firstspace = cmd:find(" ")
    if firstspace then
      findme = cmd:sub(0, firstspace-1)
    end
    local run_cmd = string.format("pgrep -u $USER -x %s > /dev/null || (%s)", findme, cmd)
    awful.spawn.with_shell(run_cmd)
  end
end

run_once({
      "urxvtd",
      "unclutter -root",
      "redshift",
})
