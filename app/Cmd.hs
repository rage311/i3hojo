{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cmd where

import qualified Data.Text as T
import System.Process
import Plugin

data Cmd = Cmd {
  cmd          :: FilePath,
  args         :: [String],
  stdinContent :: String
}

instance Plugin Cmd where
  clickPlugin :: Cmd -> IO ()
  clickPlugin _this = return ()

  runPlugin :: Cmd -> IO PluginStatus
  --runPlugin plug@(PluginConfig { delay, getUrgency, run }) = do
  runPlugin cmd'@(Cmd { cmd, args, stdinContent }) = do
    result <- readProcess cmd args stdinContent
    -- return result
    let res = T.stripStart $ T.stripEnd $ T.pack result
    return PluginStatus { text = res, urgency = Urgent }
    --return PluginStatus { text = res, urgency = urgencyPlugin $ Just (getUrgency plug ("" :: T.Text)) }
