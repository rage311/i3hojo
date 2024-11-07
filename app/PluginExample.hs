{-# LANGUAGE NumericUnderscores #-}

module PluginExample where

import Cmd
import Plugin

initializePlugin :: Int -> ChannelData -> PluginConfig a -> PluginHandle a
initializePlugin myId cd cfg =
  PluginHandle {
    channel  = cd,
    uniqueId = myId,
    plugin   = cfg
  }

pluginDelay :: Int
pluginDelay = 2

testPlugin :: PluginConfig Cmd
testPlugin = PluginConfig {
  delay      = pluginDelay * 1_000_000,
  getUrgency = const Urgent,
  core       = Cmd {
    cmd          = "/home/matt/dev/i3hojo/tmp_plugins/df.sh",
    args         = [],
    stdinContent = ""
  }}
