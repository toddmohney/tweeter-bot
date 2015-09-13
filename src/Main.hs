{-# OPTIONS_GHC -Wall #-}
module Main where
  import qualified AppLogger         as Logger
  import CSV.CSV                     as CSV
  import qualified Config            as Config
  import Control.Applicative ((<$>), (<|>))
  import Control.Concurrent (threadDelay)
  import Data.List (intersperse)
  import Tweet.Tweet                 as T
  import qualified Tweet.TweetLogger as T
  import Tweet.TweetTree             as T
  import Tweet.TweetWriter           as T

  doTweetLoop :: T.TweetTree -> IO ()
  doTweetLoop Empty = print "No tweets in the tweet tree!"
  doTweetLoop tweetTree@Node{} = do
    tweetIndex <- nextTweetIndex
    tweetDelay <- Config.tweetDelay
    case nextTweet tweetTree tweetIndex of
      Nothing  -> print "Oh no, something went wrong!"
      (Just t) -> do
        status <- T.sendTweet t
        logTweetToDataStore t
        appendToAppLog $ show status
    threadDelay tweetDelay
    doTweetLoop tweetTree

  logTweetToDataStore :: T.Tweet -> IO ()
  logTweetToDataStore t = do
    tweetLogPath <- Config.tweetLogPath
    T.logTweet tweetLogPath t

  appendToAppLog :: String -> IO ()
  appendToAppLog str = do
    logPath <- Config.logPath
    Logger.log logPath str

  nextTweet :: T.TweetTree -> Int -> Maybe T.Tweet
  nextTweet tree index = T.findTweet index tree <|> T.findFirstTweet tree

  nextTweetIndex :: IO Int
  nextTweetIndex = do
    tweetLogPath <- Config.tweetLogPath
    lastTweet    <- T.getLastLoggedTweet tweetLogPath
    case lastTweet of
      Nothing -> return 1
      (Just t) -> return $ (getIndex t) + 1

  main :: IO ()
  main = do
    tweetFilePath <- Config.tweetFilePath
    csvData       <- parseCSV <$> readFile tweetFilePath
    case csvData of
      Left e  -> appendToAppLog $ show e
      Right t ->
        let tweetParseResults       = T.buildTweetWriter t
            tweets                  = T.parseTweetsFromWriter tweetParseResults
            tweetParsingLogMessages = T.parseResultsFromWriter tweetParseResults
            formattedLogMessages    = concat $ intersperse "\n" tweetParsingLogMessages
         in
          (appendToAppLog formattedLogMessages) >> (doTweetLoop . T.buildTweetTree $ tweets)

