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

  doTweetLoop :: Maybe T.Tweet -> T.TweetTree -> IO ()
  doTweetLoop _ Empty = print "No tweets in the tweet tree!"

  doTweetLoop Nothing tweetTree@Node{} = do
    nextTweetIndex <- getNextTweetIndex
    doTweetLoop (getTweet tweetTree nextTweetIndex) tweetTree

  doTweetLoop (Just tweet) tweetTree@Node{} = do
    tweetDelay <- getTweetDelay tweet
    status     <- T.sendMockTweet tweet   -- send tweet
    logTweetToDataStore tweet             -- record most recent Tweet
    appendToAppLog $ show status          -- record status of API request to app log
    threadDelay $ tweetDelay              -- sleep for a bit

    nextTweetIndex <- getNextTweetIndex
    doTweetLoop (getTweet tweetTree nextTweetIndex) tweetTree

  getTweetDelay :: T.Tweet -> IO Int
  getTweetDelay tweet
    | (hasRelatedTweet tweet) = Config.relatedTweetDelay
    | otherwise               = Config.tweetDelay

  logTweetToDataStore :: T.Tweet -> IO ()
  logTweetToDataStore t = do
    tweetLogPath <- Config.tweetLogPath
    T.logTweet tweetLogPath t

  appendToAppLog :: String -> IO ()
  appendToAppLog str = do
    logPath <- Config.logPath
    Logger.log logPath str

  getTweet :: T.TweetTree -> Int -> Maybe T.Tweet
  getTweet tree idx = T.findTweet idx tree <|> T.findFirstTweet tree

  getNextTweetIndex :: IO Int
  getNextTweetIndex = do
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
            tweetTree               = T.buildTweetTree $ tweets
         in
         do
           appendToAppLog formattedLogMessages

           nextTweetIndex <- getNextTweetIndex
           doTweetLoop (getTweet tweetTree nextTweetIndex) tweetTree


