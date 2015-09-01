{-# OPTIONS_GHC -Wall #-}
module Main where
  import CSV
  import Control.Applicative
  import Data.Maybe (mapMaybe)
  import Tweet
  import TweetTree

  -- move to env var
  tweetFilePath :: String
  tweetFilePath = "/Users/toddmohney/workspace/tweeter-bot/src/TweetPile.csv"

  sendTweet :: Tweet -> IO ()
  sendTweet = print . getTweet

  doTweetLoop :: TweetTree -> IO ()
  doTweetLoop tweetTree = do
    case findTweet 2 tweetTree of
      Nothing -> print "No tweet found!"
      (Just tweet) -> sendTweet tweet

  main :: IO ()
  main = do
    csvData <- parseCSV <$> readFile tweetFilePath
    case csvData of
      Left e -> printCSVParserError e
      Right r -> doTweetLoop $ foldr insertTweet Empty $ mapMaybe parseTweet r



