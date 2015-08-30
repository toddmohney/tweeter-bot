module Main where
  import Control.Applicative

  data TweetTree = Empty
                   | Node TweetTree Tweet TweetTree

  data Tweet = Tweet { getIndex :: Int
                     , getTweet :: String }

  -- move to env var
  tweetFilePath :: String
  tweetFilePath = "/Users/toddmohney/workspace/tweeter-bot/src/TweetPile"

  -- use buildTweetFromString and return IO [Tweet]
  getTweetsFromFile :: IO [String]
  getTweetsFromFile = lines <$> readFile tweetFilePath

  buildTweetFromString :: String -> Tweet
  buildTweetFromString = undefined

  buildTweetTree :: [Tweet]
  buildTweetTree = undefined

  findTweet :: TweetTree -> Int -> String
  findTweet = undefined

  main :: IO ()
  main = putStr "hi"
    {- tweet <- getTweet buildTweetTree 1 -}
    {- putStr tweet -}
