{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Plugin where

import Control.Concurrent
import Control.Monad (forever)

type ChannelData = (MVar (Int, PluginStatus))

data Urgency =
  Normal
  | Important
  | Urgent
  | Critical
  deriving (Show, Eq)

urgencyColor :: Urgency -> String
urgencyColor urg =
  case urg of
    Normal    -> "#aaaaaa"
    Important -> "#dddddd"
    Urgent    -> "#ffffff"
    Critical  -> "#ff0000"

data PluginStatus = PluginStatus {
  icon    :: String,
  text    :: String,
  urgency :: Urgency
} deriving Eq

instance Show PluginStatus where
  show (PluginStatus { icon, text, urgency }) =
    let fullText = icon <> " " <> text
    in
      "\"full_text\":\""
        <> fullText
        <> "\","
      <> "\"color\":\""
        <> urgencyColor urgency
        <> "\""

type MouseBtn = Int

data Plugin = Plugin {
  click  :: MouseBtn -> IO (),
  delay  :: Int,
  status :: IO PluginStatus
}

runPlugin :: Plugin -> Int -> ChannelData -> IO ()
runPlugin plugin myId cd = forever $ do
  stat <- status plugin
  putMVar cd (myId, stat)
  threadDelay $ delay plugin * 1_000_000
