{-# OPTIONS_GHC -Wall #-}
module TweetLogger where
  import Control.Applicative ((<$>))

  logTweet :: FilePath -> String -> IO ()
  logTweet = appendFile

  getLastLogItem :: String -> IO String
  getLastLogItem path = (last . lines) <$> readFile path
