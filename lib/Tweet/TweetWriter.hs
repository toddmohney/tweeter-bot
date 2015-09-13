module Tweet.TweetWriter (
  parseTweetsFromWriter
  , parseResultsFromWriter
  , buildTweetWriter) where

  import Control.Monad.Writer
  import Data.Maybe (mapMaybe)
  import Tweet.Tweet

  type TweetWriter = ([Maybe Tweet], [String])

  parseTweetsFromWriter :: TweetWriter -> [Tweet]
  parseTweetsFromWriter writer = mapMaybe id $ fst writer

  parseResultsFromWriter :: TweetWriter -> [String]
  parseResultsFromWriter writer = snd writer

  buildTweetWriter :: [[String]] -> TweetWriter
  buildTweetWriter tweetList = runWriter $ mapM parseTweetWithLog tweetList


