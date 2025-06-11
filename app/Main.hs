{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Data.ByteString.Lazy.UTF8 as BLU
import Data.Time
import Control.Concurrent
import Control.Monad
import Data.List
import GHC.Generics
import System.IO
import System.Process

import Plugin
import PluginConfigs (pluginConfigs)

i3StaticHeader :: String
i3StaticHeader = "{\"version\":1,\"click_events\":true}"

i3FullOutput :: [(Int, String)] -> String
i3FullOutput widgets =
  "[" <> "{" <> intercalate "},{" (map fullWidget widgets) <> "}],\n"
  where
    fullWidget (wId, wText) =
      instName wId <> if null wText
        then ""
        else "," <> wText
    instName wId = "\"name\":\"" <> show wId <> "\""

readStat :: ChannelData -> IO (Int, String)
readStat cd = do
  (plugId, stat) <- takeMVar cd
  return (plugId, show stat)

doLoop :: ChannelData -> [(Int, String)] -> IO ()
doLoop cd plugins = do
  (plugId, stat) <- readStat cd

  -- TODO: use lookup* for this now that it's (id, text)?
  let plugins' =
        map (\(thisId, thisStat) ->
          if thisId == plugId then
            (thisId, stat)
          else
            (thisId, thisStat)
        ) plugins

  putStr $ i3FullOutput plugins'
  hFlush stdout
  doLoop cd plugins'

type PluginHandle = (Int, (IO (), MouseBtn -> IO ()))

-- {
  -- "name": "ethernet",
  -- "instance": "eth0",
  -- "button": 1,
  -- "modifiers": ["Shift", "Mod1"],
  -- "x": 1925,
  -- "y": 1400,
  -- "relative_x": 12,
  -- "relative_y": 8,
  -- "output_x": 5,
  -- "output_y": 1400,
  -- "width": 50,
  -- "height": 22
-- }
data ClickEvent = ClickEvent {
  name       :: String,
  button     :: MouseBtn,
  modifiers  :: [String],
  x          :: Int,
  y          :: Int,
  relative_x :: Int,
  relative_y :: Int,
  output_x   :: Int,
  output_y   :: Int,
  width      :: Int,
  height     :: Int
} deriving (Generic, Show)

instance FromJSON ClickEvent

listenClick :: [PluginHandle] -> IO ()
listenClick handles = forever $ do
  json <- getLine
  let json' = dropWhile (== ',') json
  _hnd <- spawnProcess "/usr/bin/notify-send" [ "i3hojo", json' ]
  let event = Data.Aeson.decode (BLU.fromString json') :: Maybe ClickEvent
  case event of
    Just ev -> do
      _hnd <- spawnProcess "/usr/bin/notify-send" [ "i3hojo", "event decoded" ]
      snd (snd $ handles !! read (name ev)) (button ev)
    _ -> do
      _hnd <- spawnProcess "/usr/bin/notify-send" [ "i3hojo", "event failed to be decoded" ]
      return ()


main :: IO ()
main = do
  hSetEncoding stdout utf8
  -- i3bar header
  putStrLn i3StaticHeader
  putStrLn "["
  hFlush stdout

  -- FOR TESTING
  -- putStrLn "[{\"full_text\":\" MST Wed 11/20 15:09\"}],"
  -- hFlush stdout

  -- putStrLn "[{\"full_text\":\"L 100%\"},{\"full_text\":\"/ 6.6G\"},{\"full_text\":\"D MST Wed 11/20 15:21\"}],"
  -- hFlush stdout
  -- threadDelay 10000000

  cd <- newEmptyMVar

  -- TODO: combine status + click into single entity?
  -- e.g. [(0, (runPlugin 0 ..., clickPlugin 0 ...), (1, ...)]
  let handles =
        zipWith (\myId plug ->
          (myId, (
            runPlugin plug myId cd,
            clickPlugin plug
          ))
        ) [0..] pluginConfigs

  -- let clickHandles =
  --       zipWith (\plug myId ->
  --         runPlugin plug myId cd
  --       ) pluginConfigs [0..]

  -- goForkYourselfTshirts
  _threadIds <- mapM (\(_, (plugRun, _)) ->
      forkFinally plugRun
        (\case
          Left ex  -> do print ex
          Right () -> do putStrLn "Success? (Should never happen?)"
        )
    ) handles

  -- TODO: fork stdin listener for click handle loop
  _threadId <- forkIO $ listenClick handles

  doLoop cd $ map (\(pId, _) -> (pId, "")) handles
  return ()
