module PluginConfigs where

import Data.Char
import Plugin
import Cmd

pluginConfigs :: [Plugin]
pluginConfigs = [
      Plugin {
        click  = \_ -> return (),
        delay  = 5,
        status = do
          (_exitCode, out, _err)
            <- trimCmd (Cmd {
              command = "light",
              args = [],
              stdinContent = ""
            })
          return PluginStatus {
            icon = [chr 0xf0eb],
            text = out <> "%",
            urgency = Normal
          }
      },
      Plugin {
        click  = \_ -> return (),
        delay  = 3,
        status = do
          (_exitCode, out, _err)
            <- trimCmd (Cmd {
              command = "/home/matt/dev/i3hojo/tmp_plugins/df.sh",
              args = [],
              stdinContent = ""
            })
          return PluginStatus { icon = "/", text = out, urgency = Normal }
      },
      Plugin {
        click  = \_ -> return (),
        delay  = 5,
        status = do
          (_exitCode, out, _err)
            <- trimCmd (Cmd {
              command = "date",
              args = [ "+%Z %a %m/%d %H:%M" ],
              stdinContent = ""
            })
          return PluginStatus { icon = [chr 0xf073], text = out, urgency = Normal }
      }
    ]
