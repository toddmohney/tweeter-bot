{-# OPTIONS_GHC -Wall #-}
module TweetTree where
  import Tweet

  data TweetTree = Empty
                   | Node TweetTree Tweet TweetTree
                   deriving (Show)


  insertTweet :: Tweet -> TweetTree -> TweetTree
  insertTweet tweet Empty = Node Empty tweet Empty
  insertTweet tweet@(Tweet _ _) tree@(Node l t r) 
    | tweet == t = tree
    | tweet < t = Node (insertTweet tweet l) t r
    | tweet > t = Node l t (insertTweet tweet r)
  insertTweet (Tweet _ _) (Node _ _ _) = undefined
