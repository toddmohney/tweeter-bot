{-# OPTIONS_GHC -Wall #-}
module Tweet.Tweet where
  import Control.Monad.Writer
  import Data.List (intersperse)
  import Data.Maybe
  import qualified Data.Text           as T
  import Safe (readMay)
  import qualified Tweet.API.Twitter   as API
  import Web.Twitter.Types

  data Tweet = Tweet { index :: Int
                     , content :: String
                     , relatedTweet :: Maybe Int 
                     } deriving (Show)

  instance Eq Tweet where
    a == b = getIndex a == getIndex b

  instance Ord Tweet where
    compare (Tweet idx1 _ _) (Tweet idx2 _ _) = compare idx1 idx2

  sendTweet :: Tweet -> IO Web.Twitter.Types.Status
  sendTweet t = do
    putStrLn $ "Posting message: " ++ getContent t
    API.tweet $ T.pack . getContent $ t

  -- prints to screen instead of posing to Twitter API
  -- used for local development
  sendMockTweet :: Tweet -> IO Tweet
  sendMockTweet t = (print . getContent $ t) >> return t

  getIndex :: Tweet -> Int
  getIndex = index

  getContent :: Tweet -> String
  getContent = content

  getRelatedTweet :: Tweet -> Maybe Int
  getRelatedTweet = relatedTweet

  hasRelatedTweet :: Tweet -> Bool
  hasRelatedTweet (Tweet _ _ (Just _)) = True
  hasRelatedTweet (Tweet _ _ Nothing)    = False

  toCSV :: Tweet -> String
  toCSV t = show (getIndex t) ++ "," ++ getContent t ++ "," ++ relTweet
    where 
      relTweet = do
        case (relatedTweet t) of
          Nothing   -> ""
          (Just rt) -> show rt

  parseTweet :: [String] -> Maybe Tweet
  parseTweet [] = Nothing
  parseTweet [_] = Nothing
  parseTweet (i:c:rt:[])
    | isJust safeIndex = Just $ Tweet (fromJust safeIndex) c (readMay rt)
    | otherwise = Nothing
      where 
        safeIndex = readMay i
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
