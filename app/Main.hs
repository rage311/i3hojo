{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
--{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad
import Data.List
import System.IO

import Plugin
import PluginConfigs (pluginConfigs)

i3StaticHeader :: String
i3StaticHeader = "{\"version\":1}"

i3FullOutput :: [String] -> String
i3FullOutput widgets =
  "[" <> "{" <> intercalate "},{" widgets <> "}],\n"

readStat :: ChannelData -> IO (Int, String)
readStat cd = do
  (plugId, stat) <- takeMVar cd
  return (plugId, show stat)


doLoop :: ChannelData -> [String] -> IO ()
doLoop cd plugins = forever $ do
  (plugId, stat) <- readStat cd

  let plugins' =
        zipWith (\thisId thisStat ->
          if thisId == plugId then
            stat
          else
            thisStat
        ) [0..] plugins

  putStr $ i3FullOutput plugins'
  hFlush stdout
  doLoop cd plugins'

main :: IO ()
main = do
  -- i3bar header
  putStrLn i3StaticHeader
  putStrLn "["
  hFlush stdout

  cd <- newEmptyMVar

  let handles = zipWith (\plug myId -> runPlugin plug myId cd) pluginConfigs [0..]

  -- goForkYourselfTshirts
  _threadIds <- mapM (\p ->
      forkFinally
        p
        (\case
            Left ex  -> do print ex
            Right () -> do putStrLn "Success? (Should never happen)"
        )
    ) handles

  doLoop cd $ replicate (length handles) ""
  return ()

