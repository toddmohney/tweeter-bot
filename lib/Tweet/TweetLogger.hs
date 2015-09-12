{-# OPTIONS_GHC -Wall #-}
module Tweet.TweetLogger 
  (logTweet
   , getLastLoggedTweet
   ) where

  import Control.Applicative ((<$>))
  import CSV.CSV as CSV
  import Tweet.Tweet as Tweet

  logTweet :: FilePath -> Tweet -> IO ()
  logTweet fp t = appendFile fp $ ensureEndsWithNewline $ toCSV t

  getLastLoggedTweet :: FilePath -> IO (Maybe Tweet)
  getLastLoggedTweet fp = do
    lastItem <- getLastLogItem fp
    case parseCSV lastItem of
      Left _         -> return Nothing
      Right tweetCSV -> return $ parseTweetFromCSV tweetCSV

  parseTweetFromCSV :: [[String]] -> Maybe Tweet
  parseTweetFromCSV csv
    | length csv == 0 = Nothing
    | otherwise       = parseTweet $ head csv

  getLastLogItem :: String -> IO String
  getLastLogItem path = (last . lines) <$> readFile path

  ensureEndsWithNewline :: String -> String
  ensureEndsWithNewline s
    | endsWithNewLine s = s
    | otherwise = s ++ "\r\n"

  endsWithNewLine :: String -> Bool
  endsWithNewLine s 
    | length s == 0                     = False
    | length s >= 1 && last s == '\n'   = True
    | length s >= 1 && last s == '\r'   = True
    | otherwise                         = False

