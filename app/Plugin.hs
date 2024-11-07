{-# LANGUAGE OverloadedStrings #-}

module Plugin where

import Control.Concurrent
import qualified Data.Text as T

newtype ChannelData = ChannelData (MVar (Int, PluginStatus))

data Urgency =
  Normal
  | Important
  | Urgent
  | Critical
  deriving (Show, Eq)

-- TODO: actual colors
urgencyColor :: Urgency -> T.Text
urgencyColor urg =
  case urg of
    Normal    -> "#aaaaaa"
    Important -> "#dddddd"
    Urgent    -> "#ffffff"
    Critical  -> "#ff0000"


-- TODO: "click" config
-- user-defined config
data PluginConfig a = PluginConfig {
  delay      :: Int,
  getUrgency :: Maybe (T.Text -> Urgency, T.Text) -> Urgency,
  core       :: a
}

data PluginHandle a = PluginHandle {
  uniqueId :: Int,
  channel  :: ChannelData,
  plugin   :: PluginConfig a
}

data PluginStatus = PluginStatus {
  text    :: T.Text,
  urgency :: Urgency
} deriving (Show, Eq)

class Plugin a where
  --runPlugin     :: PluginConfig a -> IO PluginStatus
  runPlugin     :: a -> IO PluginStatus
  clickPlugin   :: a -> IO ()
  --urgencyPlugin :: (T.Text -> Urgency) -> T.Text -> Urgency

urgencyPlugin :: Maybe (T.Text -> Urgency, T.Text) -> Urgency
urgencyPlugin (Just (f, x)) = f x
urgencyPlugin Nothing       = Normal

