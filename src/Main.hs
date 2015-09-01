{-# OPTIONS_GHC -Wall #-}
module Main where
  import CSV
  import Control.Applicative
  import Data.Maybe (mapMaybe)
  import Tweet
  import TweetTree

  -- move to env var
  tweetFilePath :: String
  tweetFilePath = "/Users/toddmohney/workspace/tweeter-bot/src/TweetPile.csv"

  main :: IO ()
  main = do
    csvData <- parseCSV <$> readFile tweetFilePath
    case csvData of
      Left e -> do putStrLn "Ah crap! Unable to parse CSV file: "
                   print e
      Right r -> print $ foldr insertTweet Empty $ mapMaybe parseTweet r



