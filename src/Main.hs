{-# OPTIONS_GHC -Wall #-}
module Main where
  import qualified AppLogger as Logger
  import CSV.CSV as CSV
  import Control.Applicative
  import Control.Monad.Writer
  import Data.List (intersperse)
  import Data.Maybe (mapMaybe)
  import Tweet.Tweet as Tweet
  import qualified Tweet.TweetLogger as TL
  import Tweet.TweetTree as TweetTree

  -- move to env var
  tweetFilePath :: String
  tweetFilePath = "/Users/gust/workspace/tweeter-bot/src/TweetPile.csv"
  -- move to env var
  tweetLogPath :: String
  tweetLogPath = "/Users/gust/workspace/tweeter-bot/log/tweet-log.csv"
  -- move to env var
  logPath :: String
  logPath = "/Users/gust/workspace/tweeter-bot/log/app.log"

  doTweetLoop :: TT.TweetTree -> IO ()
  doTweetLoop TT.Empty = print "No tweets in the tweet tree!"
  doTweetLoop tweetTree@TT.Node{} = do
    tweetIndex <- nextTweetIndex
    case nextTweet tweetTree tweetIndex of
      Nothing  -> print "Oh no, something went wrong!"
      (Just t) -> sendTweet t >>= logTweet

  -- placeholder strategy until we drop in actual tweeting
  sendTweet :: T.Tweet -> IO (Either String T.Tweet)
  sendTweet t = (print . T.getTweet $ t) >> (return (Right t))

  -- TODO: handle failure
  logTweet :: Either String T.Tweet -> IO ()
  logTweet (Left _) = undefined
  logTweet (Right t) = TL.logTweet tweetLogPath t

  nextTweet :: TT.TweetTree -> Int -> Maybe T.Tweet
  nextTweet tree index = TT.findTweet index tree <|> TT.findFirstTweet tree

  nextTweetIndex :: IO Int
  nextTweetIndex = do
    lastTweet <- TL.getLastLoggedTweet tweetLogPath 
    case lastTweet of
      Nothing -> return 1
      (Just t) -> return $ (T.getIndex t) + 1

  parseTweetWithLog :: [String] -> Writer [String] (Maybe T.Tweet)
  parseTweetWithLog csvTweetStr = 
    let maybeTweet = T.parseTweet csvTweetStr in
        case maybeTweet of
          Nothing  -> do
            tell ["Error parsing CSV string: " ++ (concat $ intersperse ", " csvTweetStr)]
            return Nothing
          (Just t) -> do
            tell ["Success fully parsed tweet: " ++ (show . T.getIndex $ t)]
            return (Just t)

  parseTweetsFromResults :: ([Maybe T.Tweet], [String]) -> [T.Tweet]
  parseTweetsFromResults result = mapMaybe id $ fst result

  parseResultsFromResults :: ([Maybe T.Tweet], [String]) -> [String]
  parseResultsFromResults result = snd result

  buildTweetWriter :: [[String]] -> ([Maybe T.Tweet], [String])
  buildTweetWriter tweetList = runWriter $ mapM parseTweetWithLog tweetList

  buildTweetTree :: [T.Tweet] -> TT.TweetTree
  buildTweetTree tweets = foldr TT.insertTweet TT.Empty tweets

  main :: IO ()
  main = do
    csvData <- parseCSV <$> readFile tweetFilePath
    case csvData of
      Left e  -> Logger.log logPath $ show e
      Right t ->
        let tweetParseResults = buildTweetWriter t
            tweets = parseTweetsFromResults tweetParseResults
            tweetParsingLogMessages = parseResultsFromResults tweetParseResults
            formattedLogMessages = concat $ intersperse "\n" tweetParsingLogMessages
         in
          (Logger.log logPath formattedLogMessages) >> (doTweetLoop . buildTweetTree $ tweets)

