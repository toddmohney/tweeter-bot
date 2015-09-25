module Config where
  import Control.Applicative ((<$>))
  import System.Environment (getEnv)

  logPath :: IO String
  logPath = getEnv "TWEETBOT_LOG_PATH"

  tweetFilePath :: IO String
  tweetFilePath = getEnv "TWEETBOT_FILE_PATH"

  tweetLogPath :: IO String
  tweetLogPath = getEnv "TWEETBOT_TWEET_LOG_PATH"

  tweetDelay :: IO Int
  tweetDelay = read <$> getEnv "TWEETBOT_TWEET_DELAY"

  relatedTweetDelay :: IO Int
  relatedTweetDelay = read <$> getEnv "TWEETBOT_TWEET_DELAY_BTW_RELATED_TWEETS"
