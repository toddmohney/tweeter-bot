{-# OPTIONS_GHC -Wall #-}
module Tweet where
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

