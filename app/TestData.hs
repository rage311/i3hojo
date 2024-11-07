{-# LANGUAGE NumericUnderscores #-}

module TestData where

import Cmd
import Plugin

-- DATA
-- testPlugins :: ChannelData -> [PluginHandle a]
-- testPlugins cd =
--   zipWith (\pId cfg ->
--     PluginHandle {
--       channel  = cd,
--       uniqueId = pId,
--       plugin   = cfg
--     })
--     [0..]
--     cfgs
--   where
--     cfgs = [
--         PluginConfig {
--           delay = 2_000_000,
--           getUrgency = const Urgent,
--           run = runPlugin Cmd {
--             cmd = "/home/matt/dev/i3hojo/tmp_plugins/df.sh",
--             args = [],
--             stdinContent = ""
--           }
--         },
--         PluginConfig {
--           delay = 3_000_000,
--           getUrgency = const Normal,
--           run = runPlugin Cmd {
--             cmd = "date",
--             args = [],
--             stdinContent = ""
--           }
--         },
--         PluginConfig {
--           delay = 4_000_000,
--           getUrgency = const Normal,
--           run = runPlugin Cmd {
--             cmd = "date",
--             args = ["-u"],
--             stdinContent = ""
--           }
--         }
--       ]

-- newCmd :: PluginConfig
-- newCmd =
--   PluginConfig {
--     delay = 2_000_000,
--     run = runPlugin  Cmd {
--       cmd = "/home/matt/dev/i3hojo/tmp_plugins/df.sh",
--       args = [],
--       stdinContent = ""
--     }
--   }

