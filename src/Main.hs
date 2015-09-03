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

  -- TODO: handle failure
  logTweet :: Either String Tweet -> IO ()
  logTweet (Left _) = undefined
  logTweet (Right t) = TL.logTweet tweetLogPath t

  doTweetLoop :: TweetTree -> IO ()
  doTweetLoop tweetTree = do
    tweetIndex <- nextTweetIndex
    case findTweet tweetIndex tweetTree of
      Nothing -> print "No tweet found!"
      (Just tweet) -> sendTweet tweet >>= logTweet

  nextTweetIndex :: IO Int
  nextTweetIndex = do
    lastTweet <- TL.getLastLoggedTweet tweetLogPath 
    case lastTweet of
      Nothing -> return 1
      (Just t) -> return $ (getIndex t) + 1

  main :: IO ()
  main = do
    csvData <- parseCSV <$> readFile tweetFilePath
    case csvData of
      Left e  -> printCSVParserError e
      Right t -> doTweetLoop tweetTree 
        where
          tweetTree = foldr insertTweet Empty $ mapMaybe parseTweet t

