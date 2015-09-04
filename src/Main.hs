{-# OPTIONS_GHC -Wall #-}
module Main where
  import CSV
  import Control.Applicative
  import Control.Monad.Writer
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
        do
          case maybeTweet of
            Nothing  -> do
              tell ["Error parsing CSV string: " ++ concat csvTweetStr]
              return Nothing
            (Just t) -> do
              tell ["Success fully parsed tweet: " ++ (show . getIndex $ t)]
              return (Just t)

  parseTweets :: [[String]] -> [Tweet]
  parseTweets = mapMaybe parseTweet

  buildTweetTree :: [Tweet] -> TweetTree
  buildTweetTree tweets = foldr insertTweet Empty tweets

  main :: IO ()
  main = do
    csvData <- parseCSV <$> readFile tweetFilePath
    case csvData of
      Left e  -> printCSVParserError e
      Right t -> doTweetLoop . buildTweetTree . parseTweets $ t

