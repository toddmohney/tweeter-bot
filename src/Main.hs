{-# OPTIONS_GHC -Wall #-}
module Main where
  import qualified AppLogger as Logger
  import CSV
  import Control.Applicative
  import Control.Monad.Writer
  import Data.List (intersperse)
  import Data.Maybe (mapMaybe)
  import Tweet
  import qualified TweetLogger as TL
  import TweetTree

  -- move to env var
  tweetFilePath :: String
  tweetFilePath = "/Users/toddmohney/workspace/tweeter-bot/src/TweetPile.csv"
  -- move to env var
  tweetLogPath :: String
  tweetLogPath = "/Users/toddmohney/workspace/tweeter-bot/log/tweet-log.csv"
  -- move to env var
  logPath :: String
  logPath = "/Users/toddmohney/workspace/tweeter-bot/log/app.log"

  -- placeholder strategy until we drop in actual tweeting
  sendTweet :: Tweet -> IO (Either String Tweet)
  sendTweet t = (print . getTweet $ t) >> (return (Right t))

  -- TODO: handle failure
  logTweet :: Either String Tweet -> IO ()
  logTweet (Left _) = undefined
  logTweet (Right t) = TL.logTweet tweetLogPath t

  doTweetLoop :: TweetTree -> IO ()
  doTweetLoop Empty = print "No tweets in the tweet tree!"
  doTweetLoop tweetTree@Node{} = do
    tweetIndex <- nextTweetIndex
    case nextTweet tweetTree tweetIndex of
      Nothing  -> print "Oh no, something went wrong!"
      (Just t) -> sendTweet t >>= logTweet

  nextTweet :: TweetTree -> Int -> Maybe Tweet
  nextTweet tree index = findTweet index tree <|> findFirstTweet tree

  nextTweetIndex :: IO Int
  nextTweetIndex = do
    lastTweet <- TL.getLastLoggedTweet tweetLogPath 
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

