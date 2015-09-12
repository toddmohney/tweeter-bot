module Tweet.TweetWriter (
  parseTweetsFromWriter
  , parseResultsFromWriter
  , buildTweetWriter) where

  import Control.Monad.Writer
  import Data.Maybe (mapMaybe)
  import Tweet.Tweet

  type TweetWriter = ([Maybe Tweet], [String])

  parseTweetsFromWriter :: TweetWriter -> [Tweet]
  parseTweetsFromWriter result = mapMaybe id $ fst result

  parseResultsFromWriter :: TweetWriter -> [String]
  parseResultsFromWriter result = snd result

  buildTweetWriter :: [[String]] -> TweetWriter
  buildTweetWriter tweetList = runWriter $ mapM parseTweetWithLog tweetList


