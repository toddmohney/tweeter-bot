{-# OPTIONS_GHC -Wall #-}
module Tweet.Tweet where
  import Control.Monad.Writer
  import Data.List (intersperse)
  import Data.Maybe
  import Safe (readMay)

  data Tweet = Tweet { getIndex :: Int
                     , getTweet :: String }
                     deriving (Show)

  instance Eq Tweet where
    a == b = getIndex a == getIndex b

  instance Ord Tweet where
    compare (Tweet idx1 _) (Tweet idx2 _) = compare idx1 idx2

  toCSV :: Tweet -> String
  toCSV t = show (getIndex t) ++ "," ++ getTweet t

  parseTweet :: [String] -> Maybe Tweet
  parseTweet [] = Nothing
  parseTweet [_] = Nothing
  parseTweet (idx:tweet:[])
    | isJust index = Just $ Tweet (fromJust index) tweet
    | otherwise = Nothing
      where 
        index = readMay idx
  parseTweet (_:_:_) = Nothing

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
