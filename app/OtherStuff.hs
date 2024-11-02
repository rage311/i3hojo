
-- TODO: needs config here
-- lupita :: IO ()
-- lupita = do
--   putStrLn $ fullOutput [widget widget0 Nothing]
--   hFlush stdout
--   threadDelay 1_000_000
--   lupita

-- waitReturn :: String -> IO String
-- waitReturn s = do
--   _ <- threadDelay 1_000_000
--   return s

-- async' :: IO a -> IO (MVar a)
-- async' action = do
--   var <- newEmptyMVar
--   res <- action
--   putMVar var res
--   return var

-- subP :: Int -> IO a -> IO (Async a)
-- subP channel timeout action = forever $ do
--   result <- action
--   putMVar channel result
--   threadDelay timeout
--   return channel

-- ioString :: String -> IO String
-- ioString = return

-- newtype Async a = Async (MVar a)

-- await :: Async a -> IO a
-- await (Async var) = readMVar var

