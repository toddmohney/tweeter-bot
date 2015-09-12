{-# OPTIONS_GHC -Wall #-}
module Main where
  import qualified AppLogger         as Logger
  import CSV.CSV                     as CSV
  import qualified Config            as Config
  import Control.Applicative
  import Data.List (intersperse)
  import Tweet.Tweet                 as Tweet
  import qualified Tweet.TweetLogger as TweetLogger
  import Tweet.TweetTree             as TweetTree
  import Tweet.TweetWriter           as TweetWriter

  doTweetLoop :: TweetTree -> IO ()
  doTweetLoop Empty = print "No tweets in the tweet tree!"
  doTweetLoop tweetTree@Node{} = do
    tweetIndex <- nextTweetIndex
    case nextTweet tweetTree tweetIndex of
      Nothing  -> print "Oh no, something went wrong!"
      (Just t) -> sendTweet t >>= logTweet

  -- placeholder strategy until we drop in actual tweeting
  sendTweet :: Tweet -> IO (Either String Tweet)
  sendTweet t = (print . getTweet $ t) >> (return (Right t))

  -- TODO: handle failure
  logTweet :: Either String Tweet -> IO ()
  logTweet (Left _) = undefined
  logTweet (Right t) = do
    tweetLogPath <- Config.tweetLogPath
    TweetLogger.logTweet tweetLogPath t

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
      Left e  -> Logger.log logPath $ show e
      Right t ->
        let tweetParseResults       = buildTweetWriter t
            tweets                  = parseTweetsFromWriter tweetParseResults
            tweetParsingLogMessages = parseResultsFromWriter tweetParseResults
            formattedLogMessages    = concat $ intersperse "\n" tweetParsingLogMessages
         in
          (Logger.log logPath formattedLogMessages) >> (doTweetLoop . buildTweetTree $ tweets)

