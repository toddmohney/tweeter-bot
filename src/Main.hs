{-# OPTIONS_GHC -Wall #-}
module Main where
  import qualified AppLogger as Logger
  import CSV.CSV as CSV
  import Control.Applicative
  import Control.Monad.Writer
  import Data.List (intersperse)
  import Data.Maybe (mapMaybe)
  import System.Environment (getEnv)
  import Tweet.Tweet as Tweet
  import qualified Tweet.TweetLogger as TweetLogger
  import Tweet.TweetTree as TweetTree

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
    tweetLogPath <- getEnv "TWEETBOT_TWEET_LOG_PATH"
    TweetLogger.logTweet tweetLogPath t

  nextTweet :: TweetTree -> Int -> Maybe Tweet
  nextTweet tree index = findTweet index tree <|> findFirstTweet tree

  nextTweetIndex :: IO Int
  nextTweetIndex = do
    tweetLogPath <- getEnv "TWEETBOT_TWEET_LOG_PATH"
    lastTweet    <- TweetLogger.getLastLoggedTweet tweetLogPath 
    case lastTweet of
      Nothing -> return 1
      (Just t) -> return $ (getIndex t) + 1

  parseTweetWithLog :: [String] -> Writer [String] (Maybe Tweet)
  parseTweetWithLog csvTweetStr = 
    let maybeTweet = parseTweet csvTweetStr in
        case maybeTweet of
          Nothing  -> do
            tell ["Error parsing CSV string: " ++ (concat $ intersperse ", " csvTweetStr)]
            return Nothing
          (Just t) -> do
            tell ["Success fully parsed tweet: " ++ (show . getIndex $ t)]
            return (Just t)

  parseTweetsFromResults :: ([Maybe Tweet], [String]) -> [Tweet]
  parseTweetsFromResults result = mapMaybe id $ fst result

  parseResultsFromResults :: ([Maybe Tweet], [String]) -> [String]
  parseResultsFromResults result = snd result

  buildTweetWriter :: [[String]] -> ([Maybe Tweet], [String])
  buildTweetWriter tweetList = runWriter $ mapM parseTweetWithLog tweetList

  buildTweetTree :: [Tweet] -> TweetTree
  buildTweetTree tweets = foldr insertTweet Empty tweets

  main :: IO ()
  main = do
    logPath       <- getEnv "TWEETBOT_LOG_PATH"
    tweetFilePath <- getEnv "TWEETBOT_FILE_PATH"
    csvData       <- parseCSV <$> readFile tweetFilePath
    case csvData of
      Left e  -> Logger.log logPath $ show e
      Right t ->
        let tweetParseResults       = buildTweetWriter t
            tweets                  = parseTweetsFromResults tweetParseResults
            tweetParsingLogMessages = parseResultsFromResults tweetParseResults
            formattedLogMessages    = concat $ intersperse "\n" tweetParsingLogMessages
         in
          (Logger.log logPath formattedLogMessages) >> (doTweetLoop . buildTweetTree $ tweets)

