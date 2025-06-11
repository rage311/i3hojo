module PluginConfigs where

import Data.Char
import System.Process

import Cmd
import Plugin

-- mouseBtn' :: String -> Int
-- mouseBtn' str = case map toLower str of
--   "left"   -> 1
--   "middle" -> 2
--   "right"  -> 3
--   "up"     -> 4
--   "down"   -> 5
--   _        -> 0

-- mouseBtn :: Int -> String
-- mouseBtn btn = case btn of
--   1 -> "left"   
--   2 -> "middle" 
--   3 -> "right"  
--   4 -> "up"     
--   5 -> "down"   
--   _ -> ""

pluginConfigs :: [Plugin]
pluginConfigs = [
      Plugin {
        click  = \_ -> return (),
        delay  = 15,
        status = do
          (_exitCode, out, _err)
            <- trimCmd (Cmd {
              command = "/usr/bin/env",
              args    = [
                "perl",
                "/home/matt/dev/i3mojo/i3hojo_wrapper.pl",
                "linux_wifi"
              ],
              stdinContent = ""
            })
          let (urgency', text') = i3mojoStatus out
          -- return $ i3mojoStatus [ chr 0xf1eb ] out
          return PluginStatus {
            icon    = [ chr 0xf1eb ],
            text    = text',
            urgency = urgency'
          }
      },

      Plugin {
        click  = \_ -> return (),
        delay  = 30,
        status = do
          (_exitCode, out, _err)
            <- trimCmd (Cmd {
              command      = "/usr/bin/env",
              args         = [
                "perl",
                "/home/matt/dev/i3mojo/i3hojo_wrapper.pl",
                "linux_battery",
                "{\"sys_path\":\"/sys/class/power_supply/BAT1\"}"
              ],
              stdinContent = ""
            })
          let (urgency', text') = i3mojoStatus out
          return PluginStatus {
            icon    = "",
            text    = text',
            urgency = urgency'
          }
      },

      Plugin {
        click = \btn -> do
          case clickFromInt btn of
            MouseUp -> do
              _ <- callProcess "/usr/bin/light" [ "-A", "5" ]
              return ()
            MouseDown -> do
              _ <- callProcess "/usr/bin/light" [ "-U", "5" ]
              return ()
            _ -> return (),
        delay  = 5,
        status = do
          (_exitCode, out, _err)
            <- trimCmd (Cmd {
              command      = "light",
              args         = [],
              stdinContent = ""
            })
          return PluginStatus {
            icon    = [chr 0xf0eb],
            text    = takeWhile isDigit out <> "%",
            urgency = Normal
          }
      },

      Plugin {
        click  = \_ -> return (),
        delay  = 60,
        status = do
          (_exitCode, out, _err)
            <- trimCmd (Cmd {
              command      = "/bin/sh",
              args         = [ "/home/matt/dev/i3hojo/tmp_plugins/df.sh", "/" ],
              stdinContent = ""
            })
          return PluginStatus {
            icon = "/",
            text = out,
            urgency = Normal
          }
      },

      Plugin {
        click  = \_ -> return (),
        delay  = 60,
        status = do
          (_exitCode, out, _err)
            <- trimCmd (Cmd {
              command      = "/bin/sh",
              args         = [ "/home/matt/dev/i3hojo/tmp_plugins/df.sh", "/home" ],
              stdinContent = ""
            })
          return PluginStatus {
            icon    = [chr 0xf015],
            text = out,
            urgency = Normal
          }
      },

      Plugin {
        click = \btn -> do
          case clickFromInt btn of
            MouseLeft -> do
              _ <- callProcess "/usr/bin/pactl" [
                  "set-sink-mute",
                  "@DEFAULT_SINK",
                  "toggle"
                ]
              return ()
            MouseRight -> do
              _ <- callProcess "/usr/bin/i3-msg" [
                  "-q",
                  "--",
                  "exec",
                  "xterm",
                  "-e",
                  "pulsemixer"
                ]
              return ()
            MouseUp -> do
              _ <- callProcess "/usr/bin/pactl" [
                  "set-sink-volume",
                  "@DEFAULT_SINK",
                  "+5%"
                ]
              return ()
            MouseDown -> do
              _ <- callProcess "/usr/bin/pactl" [
                  "set-sink-volume",
                  "@DEFAULT_SINK",
                  "-5%"
                ]
              return ()
            _ -> return (),
        delay  = 30,
        status = do
          (_exitCode, out, _err)
            <- trimCmd (Cmd {
              command      = "/usr/bin/env",
              args         = [
                "perl",
                "/home/matt/dev/i3mojo/i3hojo_wrapper.pl",
                "pulseaudio"
              ],
              stdinContent = ""
            })
          let (urgency', text') = i3mojoStatus out
          return PluginStatus {
            icon    = "",
            text    = text',
            urgency = urgency'
          }
      },

      Plugin {
        click  = \_ -> return (),
        delay  = 30,
        status = do
          (_exitCode, out, _err)
            <- trimCmd (Cmd {
              command      = "date",
              args         = [ "+%Z %a %m/%d %H:%M" ],
              stdinContent = ""
            })
          return PluginStatus {
            icon    = [chr 0xf073],
            text    = out,
            urgency = Normal
          }
      }
    ]
