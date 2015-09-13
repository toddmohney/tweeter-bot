{-# OPTIONS_GHC -Wall #-}
module Tweet.API.Twitter where
  import qualified Data.Text           as T
  import Network.HTTP.Conduit (withManager)
  import Tweet.API.Config              as ApiConfig
  import qualified Web.Twitter.Conduit as TC
  import Web.Twitter.Types

  tweet :: T.Text -> IO Web.Twitter.Types.Status
  tweet t = do
    twInfo <- ApiConfig.getTWInfoFromEnv
    withManager $ \mgr -> TC.call twInfo mgr $ TC.update t
