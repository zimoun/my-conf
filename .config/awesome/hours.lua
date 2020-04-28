--- Add another hour: France
-- Function to simplify my life (or just my readings)
-- http://awesome.naquadah.org/wiki/Awesome_3_configuration#Files
-- Execute command and return its output. 
-- -- You probably won't only execute commands with one
-- -- line of output
function execute_command(command)
   local fh = io.popen(command)
   local str = ""
   for i in fh:lines() do
      str = str .. i
   end
   io.close(fh)
   return str
end

function start_other(mystring)
   local myhourparis = widget({ type = "textbox" })
   myclocktimer = timer({ timeout = 30 })
   myclocktimer:add_signal("timeout", function() myhourparis.text = "Fr( " .. execute_command("TZ='Europe/Paris' date +%H:%M") .. " )" end)
   myclocktimer:start()
end
