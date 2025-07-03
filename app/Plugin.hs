{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Plugin where

import Control.Concurrent
import Control.Monad (forever)

type ChannelData = (MVar (Int, PluginStatus))

data Urgency
  = Normal
  | Important
  | Urgent
  | Critical
  deriving (Enum, Eq, Show)

urgencyFromInt :: Int -> Urgency
urgencyFromInt int
  | int >= fromEnum Normal && int <= fromEnum Critical = toEnum int
  | otherwise = Normal

urgencyColor :: Urgency -> String
urgencyColor urg =
  case urg of
    Normal    -> "#aaaaaa"
    Important -> "#dddddd"
    Urgent    -> "#ffffff"
    Critical  -> "#ff0000"

data PluginStatus = PluginStatus
  { icon    :: String
  , text    :: String
  , urgency :: Urgency
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

data Plugin = Plugin
  { click  :: MouseBtn -> IO ()
  , delay  :: Int
  , status :: IO PluginStatus
  }

type ClickFn      = IO ()
type StatusFn     = MouseBtn -> IO ()
type PluginID     = Int
type PluginHandle = (PluginID, (ClickFn, StatusFn))

clickPlugin :: ChannelData -> Plugin -> PluginID -> MouseBtn -> IO ()
clickPlugin cd self myId btn = click self btn >>
  status self >>= \stat -> putMVar cd (myId, stat)

runPlugin :: ChannelData -> Plugin -> Int -> IO ()
runPlugin cd self myId = forever $ do
  stat <- status self
  putMVar cd (myId, stat)
  threadDelay $ delay self * 1_000_000

data Click
  = MouseUnknown
  | MouseLeft
  | MouseMiddle
  | MouseRight
  | MouseUp
  | MouseDown
  deriving (Enum, Eq, Show)

intFromClick :: Click -> Int
intFromClick = fromEnum

clickFromInt :: Int -> Click
clickFromInt = toEnum

i3mojoStatus :: String -> (Urgency, String)
i3mojoStatus input = (urg, text')
  where
    urg = urgencyFromInt $ read $ takeWhile (/= ':') input
    text' = dropWhile (== ':') $ dropWhile (/= ':') input

