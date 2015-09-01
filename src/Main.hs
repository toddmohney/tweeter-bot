{-# OPTIONS_GHC -Wall #-}
module Main where
  import CSV
  import Control.Applicative
  import Data.Maybe (mapMaybe)
  import Tweet
  import TweetLogger
  import TweetTree

  -- move to env var
  tweetFilePath :: String
  tweetFilePath = "/Users/toddmohney/workspace/tweeter-bot/src/TweetPile.csv"

  doTweetLoop :: TweetTree -> IO ()
  doTweetLoop tweetTree = do
    case findTweet 1 tweetTree of
      Nothing -> print "No tweet found!"
      (Just tweet) -> sendTweet tweet

  sendTweet :: Tweet -> IO ()
  sendTweet = print . getTweet

  main :: IO ()
  main = do
    csvData <- parseCSV <$> readFile tweetFilePath
    case csvData of
      Left e -> printCSVParserError e
      Right r -> doTweetLoop $ foldr insertTweet Empty $ mapMaybe parseTweet r



