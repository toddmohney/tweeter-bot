{-# OPTIONS_GHC -Wall #-}
module Main where
  import CSV
  import Control.Applicative
  import Data.Maybe (mapMaybe)
  import Tweet
  import qualified TweetLogger as TL
  import TweetTree

  -- move to env var
  tweetFilePath :: String
  tweetFilePath = "/Users/toddmohney/workspace/tweeter-bot/src/TweetPile.csv"
  -- move to env var
  tweetLogPath :: String
  tweetLogPath = "/Users/toddmohney/workspace/tweeter-bot/src/TweetLog.csv"

  -- placeholder strategy until we drop in actual tweeting
  sendTweet :: Tweet -> IO (Either String Tweet)
  sendTweet t = (print . getTweet $ t) >> (return (Right t))

  logTweet :: Either String Tweet -> IO ()
  logTweet (Left msg) = undefined
  logTweet (Right t) = TL.logTweet tweetLogPath (toCSV t)

  doTweetLoop :: TweetTree -> IO ()
  doTweetLoop tweetTree = do
    case findTweet 1 tweetTree of
      Nothing -> print "No tweet found!"
      (Just tweet) -> sendTweet tweet >>= logTweet

  main :: IO ()
  main = do
    csvData <- parseCSV <$> readFile tweetFilePath
    case csvData of
      Left e -> printCSVParserError e
      Right r -> doTweetLoop tweetTree 
        where
          tweetTree = foldr insertTweet Empty $ mapMaybe parseTweet r



