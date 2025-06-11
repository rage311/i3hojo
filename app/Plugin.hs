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

urgencyFromInt :: Int -> Urgency
urgencyFromInt int =
  case int of
    0 -> Normal
    1 -> Important
    2 -> Urgent
    3 -> Critical
    _ -> Normal

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

clickPlugin :: Plugin -> MouseBtn -> IO ()
clickPlugin = click
  -- click self
  -- return ()

runPlugin :: Plugin -> Int -> ChannelData -> IO ()
runPlugin plugin myId cd = forever $ do
  stat <- status plugin
  putMVar cd (myId, stat)
  threadDelay $ delay plugin * 1_000_000

data Click
  = MouseLeft
  | MouseMiddle
  | MouseRight
  | MouseUp
  | MouseDown
  | MouseUnknown

clickFromInt :: Int -> Click
clickFromInt btn =
  case btn of
    -- TODO: hide prelude "Right", "Left" for renaming?
    1 -> MouseLeft
    2 -> MouseMiddle
    3 -> MouseRight
    4 -> MouseUp
    5 -> MouseDown
    _ -> MouseUnknown

i3mojoStatus :: String -> (Urgency, String)
i3mojoStatus input = (urg, text')
  where
    urg = urgencyFromInt $ read $ takeWhile (/= ':') input
    text' = dropWhile (== ':') $ dropWhile (/= ':') input

