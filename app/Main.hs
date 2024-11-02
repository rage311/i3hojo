{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module Main where

import Control.Concurrent
import Control.Monad
import qualified Data.Text as T
import System.IO
import System.Process

data Urgency =
  Normal
  | Important
  | Urgent
  | Critical
  deriving (Show, Eq)

-- TODO: actual colors
urgencyColor :: Urgency -> T.Text
urgencyColor urgency =
  case urgency of
    Normal    -> "#ffffff"
    Important -> "#ffffff"
    Urgent    -> "#ffffff"
    Critical  -> "#ffffff"

data PluginStatus = PluginStatus {
  text    :: T.Text,
  urgency :: Urgency
} deriving (Show, Eq)

data Cmd = Cmd {
  cmd          :: FilePath,
  args         :: [String],
  stdinContent :: String
}

class Plugin a where
  runPlugin   :: a -> IO PluginStatus
  clickPlugin :: a -> IO ()

instance Plugin Cmd where
  clickPlugin :: Cmd -> IO ()
  clickPlugin _this = return ()

  runPlugin :: Cmd -> IO PluginStatus
  runPlugin (Cmd { cmd, args, stdinContent }) = do
    result <- readProcess cmd args stdinContent
    let res = T.stripEnd $ T.pack result
    return PluginStatus { text = res, urgency = Normal }

newtype ChannelData = ChannelData (MVar (Int, PluginStatus))

-- TODO: "click" config
data PluginConfig = PluginConfig {
  delay    :: Int,
  uniqueId :: Int,
  run      :: IO PluginStatus,
  --run      :: a -> IO PluginStatus,
  channel  :: ChannelData
}

staticHeader :: String
staticHeader = "{\"version\":1}"

widget :: PluginStatus -> T.Text
widget (PluginStatus { text, urgency }) =
  "\"full_text\":\"" <> text <> "\""
  <> ",\"color\":\"" <> urgencyColor urgency <> "\""

fullOutput :: [T.Text] -> T.Text
fullOutput widgets =
  "[" <> "{" <> T.intercalate "},{" widgets <> "}],\n"

recurring ::  PluginConfig -> IO ()
recurring cfg@(PluginConfig { delay, uniqueId, channel }) = forever $ do
  result <- run cfg
  let (ChannelData cd) = channel
  putMVar cd (uniqueId, result)
  threadDelay delay
  
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

  let plugins = testPlugins cd

  -- goForkYourselfTshirts
  _threadIds <- mapM (\p -> forkFinally
    (recurring p)
    (\case
        Left ex  -> do print ex
        Right () -> do putStrLn "Success? (Should never happen)"
    ))
    plugins

  doLoop cd $ replicate (length plugins) ""
  return ()



-- DATA
testPlugins :: ChannelData -> [PluginConfig]
testPlugins cd = [
  PluginConfig {
    channel = cd,
    run = runPlugin Cmd {
      cmd = "ls",
      args = ["-Al", "| tail -1"],
      stdinContent = ""
    },
    delay = 2_000_000,
    uniqueId = 0
  },
  PluginConfig {
    channel = cd,
    run = runPlugin Cmd {
      cmd = "date",
      args = ["-u"],
      stdinContent = ""
    },
    delay = 3_000_000,
    uniqueId = 1
  }
  ]

