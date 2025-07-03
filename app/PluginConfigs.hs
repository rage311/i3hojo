module PluginConfigs where

import Data.Char
import System.Directory
import System.Process

import Cmd
import Plugin

-- formatting to look more like JSON
pluginConfigs :: [Plugin]
pluginConfigs = [
    Plugin {
      click  = \_ -> return (),
      delay  = 15,
      status = do
        home <- getHomeDirectory
        (_exitCode, out, _err)

          <- trimCmd (Cmd {
            command = "/usr/bin/env",
            args    = [
              "perl",
              home <> "/dev/i3mojo/i3hojo_wrapper.pl",
              "linux_wifi"
            ],
            stdinContent = ""
          })
        let (urgency', text') = i3mojoStatus out
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
        home <- getHomeDirectory
        (_exitCode, out, _err)
          <- trimCmd (Cmd {
            command      = "/usr/bin/env",
            args         = [
              "perl",
              home <> "/dev/i3mojo/i3hojo_wrapper.pl",
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
        case toEnum btn of
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
        home <- getHomeDirectory
        (_exitCode, out, _err)
          <- trimCmd (Cmd {
            command      = "/bin/sh",
            args         = [ home <> "/dev/i3hojo/tmp_plugins/df.sh", "/" ],
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
        home <- getHomeDirectory
        (_exitCode, out, _err)
          <- trimCmd (Cmd {
            command      = "/bin/sh",
            args         = [ home <> "/dev/i3hojo/tmp_plugins/df.sh", "/home" ],
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
        case toEnum btn of
          MouseLeft -> do
            _ <- callProcess "/usr/bin/pactl" [
                "set-sink-mute",
                "@DEFAULT_SINK@",
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
                "@DEFAULT_SINK@",
                "+5%"
              ]
            return ()
          MouseDown -> do
            _ <- callProcess "/usr/bin/pactl" [
                "set-sink-volume",
                "@DEFAULT_SINK@",
                "-5%"
              ]
            return ()
          _ -> return (),
      delay  = 30,
      status = do
        home <- getHomeDirectory
        (_exitCode, out, _err)
          <- trimCmd (Cmd {
            command      = "/usr/bin/env",
            args         = [
              "perl",
              home <> "/dev/i3mojo/i3hojo_wrapper.pl",
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
