{-# OPTIONS_GHC -Wall #-}
module Tweet.TweetTree where
  import Tweet.Tweet as Tweet

  data TweetTree = Empty
                   | Node TweetTree Tweet TweetTree
                   deriving (Show)

  buildTweetTree :: [Tweet] -> TweetTree
  buildTweetTree tweets = foldr insertTweet Empty tweets

  findTweet :: Int -> TweetTree -> Maybe Tweet
  findTweet _ Empty = Nothing
  findTweet idx (Node l t r)
    | idx == getIndex t = Just t
    | idx < getIndex t  = findTweet idx l
    | idx > getIndex t  = findTweet idx r
  findTweet _ (Node _ _ _) = undefined

  findFirstTweet :: TweetTree -> Maybe Tweet
  findFirstTweet Empty            = Nothing
  findFirstTweet (Node Empty t _) = Just t
  findFirstTweet (Node l _ _)     = findFirstTweet l

  -- change this to a Red/Black tree, for better balancing
  insertTweet :: Tweet -> TweetTree -> TweetTree
  insertTweet tweet Empty = Node Empty tweet Empty
  insertTweet tweet@(Tweet _ _) tree@(Node l t r) 
    | tweet == t = tree
    | tweet < t = Node (insertTweet tweet l) t r
    | tweet > t = Node l t (insertTweet tweet r)
  insertTweet (Tweet _ _) (Node _ _ _) = undefined
