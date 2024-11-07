{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad
import qualified Data.Text as T
import System.IO

import Plugin
import TestData
import PluginExample (testPlugin, initializePlugin)

staticHeader :: String
staticHeader = "{\"version\":1}"

widget :: PluginStatus -> T.Text
widget (PluginStatus { text, urgency }) =
  "\"full_text\":\"" <> text <> "\""
  <> ",\"color\":\"" <> urgencyColor urgency <> "\""
  -- <> ",\"name\":\"billy\""

fullOutput :: [T.Text] -> T.Text
fullOutput widgets =
  "[" <> "{" <> T.intercalate "},{" widgets <> "}],\n"

recurring :: Plugin a => PluginHandle a -> IO ()
recurring (PluginHandle { uniqueId, channel, plugin }) =
  forever $ do
    result <- runPlugin (core plugin)
    let (ChannelData cd) = channel
    putMVar cd (uniqueId, result)
    threadDelay (delay plugin)
  
readStat :: ChannelData -> IO (Int, T.Text)
readStat (ChannelData cd) = do
  (cmdId, stat) <- takeMVar cd
  return (cmdId, widget stat)

doLoop :: ChannelData -> [T.Text] -> IO ()
doLoop cd plugins = do
  (cmdId, stat) <- readStat cd

  let plugins' =
        zipWith (\thisId thisStat ->
          if thisId == cmdId then
            stat
          else
            thisStat
        ) [0..] plugins

  putStr $ T.unpack $ fullOutput plugins'
  hFlush stdout
  doLoop cd plugins'

main :: IO ()
main = do
  -- i3bar header
  putStrLn staticHeader
  putStrLn "["
  hFlush stdout

  cd <- ChannelData <$> newEmptyMVar

  -- let plugins = testPlugins
  let hnd = initializePlugin 0 cd testPlugin

  -- goForkYourselfTshirts
  _threadIds <- mapM (\p ->
      forkFinally
        (recurring p)
        (\case
            Left ex  -> do print ex
            Right () -> do putStrLn "Success? (Should never happen)"
        )
    ) [hnd]

  --doLoop cd $ replicate (length plugins) ""
  doLoop cd $ replicate 1 ""
  return ()

