module Main where
  import CSV
  import Control.Applicative

  data TweetTree = Empty
                   | Node TweetTree Tweet TweetTree

  data Tweet = Tweet { getIndex :: Int
                     , getTweet :: String }

  -- move to env var
  tweetFilePath :: String
  tweetFilePath = "/Users/toddmohney/workspace/tweeter-bot/src/TweetPile.csv"

  parseTweet :: [String] -> Tweet
  parseTweet = undefined

  insertTweet :: Tweet -> TweetTree -> TweetTree
  insertTweet = undefined

  main :: IO ()
  main = do
    {- csvData <- parseCSV <$> readFile tweetFilePath -}
    {- case csvData of -}
      {- Left e -> do putStrLn "Ah crap! Unable to parse CSV file: " -}
                   {- print e -}
      {- Right r -> mapM_ (insertTweet . parseTweet) r -}
    putStrLn "ok, bye"
