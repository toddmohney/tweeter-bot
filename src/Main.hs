{-# OPTIONS_GHC -Wall #-}
module Main where
  import qualified AppLogger         as Logger
  import CSV.CSV                     as CSV
  import qualified Config            as Config
  import Control.Applicative ((<$>), (<|>))
  import Control.Concurrent (threadDelay)
  import Data.List (intersperse)
  import Tweet.Tweet                 as Tweet
  import qualified Tweet.TweetLogger as TweetLogger
  import Tweet.TweetTree             as TweetTree
  import Tweet.TweetWriter           as TweetWriter

  doTweetLoop :: TweetTree -> IO ()
  doTweetLoop Empty = print "No tweets in the tweet tree!"
  doTweetLoop tweetTree@Node{} = do
    tweetIndex <- nextTweetIndex
    tweetDelay <- Config.tweetDelay
    case nextTweet tweetTree tweetIndex of
      Nothing  -> print "Oh no, something went wrong!"
      (Just t) -> do
        status <- sendTweet t
        logTweetToDataStore t
        appendToAppLog $ show status
    threadDelay tweetDelay
    doTweetLoop tweetTree

  logTweetToDataStore :: Tweet -> IO ()
  logTweetToDataStore t = do
    tweetLogPath <- Config.tweetLogPath
    TweetLogger.logTweet tweetLogPath t

  appendToAppLog :: String -> IO ()
  appendToAppLog str = do
    logPath <- Config.logPath
    Logger.log logPath str

  nextTweet :: TweetTree -> Int -> Maybe Tweet
  nextTweet tree index = findTweet index tree <|> findFirstTweet tree

  nextTweetIndex :: IO Int
  nextTweetIndex = do
    tweetLogPath <- Config.tweetLogPath
    lastTweet    <- TweetLogger.getLastLoggedTweet tweetLogPath
    case lastTweet of
      Nothing -> return 1
      (Just t) -> return $ (getIndex t) + 1

  main :: IO ()
  main = do
    logPath       <- Config.logPath
    tweetFilePath <- Config.tweetFilePath
    csvData       <- parseCSV <$> readFile tweetFilePath
    case csvData of
      Left e  -> appendToAppLog $ show e
      Right t ->
        let tweetParseResults       = buildTweetWriter t
            tweets                  = parseTweetsFromWriter tweetParseResults
            tweetParsingLogMessages = parseResultsFromWriter tweetParseResults
            formattedLogMessages    = concat $ intersperse "\n" tweetParsingLogMessages
         in
          (appendToAppLog formattedLogMessages) >> (doTweetLoop . buildTweetTree $ tweets)

