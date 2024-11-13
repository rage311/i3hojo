{-# LANGUAGE NamedFieldPuns #-}

module Cmd where

import Data.Char
import System.Exit
import System.Process

type ProcessResult = (ExitCode, String, String)

data Cmd = Cmd {
  command      :: FilePath,
  args         :: [String],
  stdinContent :: String
}

trimCmd :: Cmd -> IO ProcessResult
trimCmd (Cmd { command, args, stdinContent }) = do
    (exitCode, stdout, stderr) <- readProcessWithExitCode
      command
      args
      stdinContent

    return (exitCode, trim stdout, trim stderr)
  where
    trim inp = dropWhile isSpace $ reverse $ dropWhile isSpace $ reverse inp
