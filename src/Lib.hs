{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( buildText
    , fetchLatestWeight
    , tweet
    ) where

import Control.Arrow ((&&&), arr, returnA)
import Control.Applicative (liftA2)
import Control.Lens ((^?), (^.), maximumByOf, Prism', prism')
import Data.Aeson.Lens (_Array, _Double, key, _String)
import Data.Function (on)
import Data.Text.Lens (unpacked)
import Data.Time (localDay, LocalTime, showGregorian)
import Network.Wreq (get, responseBody)
import System.Environment (getEnv)
import Text.Read (readMaybe)
import Web.Twitter.Conduit (call, Credential(..), def, newManager, OAuth(..), oauthConsumerSecret, oauthConsumerKey, setCredential, tlsManagerSettings, TWInfo(..), twitterOAuth, update)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Text as T

replace :: Eq a => a -> a -> [a] -> [a]
replace from to = map (\x -> if x == from then to else x)

_LocalTime :: Prism' String LocalTime
_LocalTime = prism' show (readMaybe . replace 'T' ' ')

fetchLatestWeight :: IO (Maybe (Double, LocalTime))
fetchLatestWeight = do
    res <- get "https://api.ryota-ka.me/weights"
    pure . maybe Nothing (uncurry (liftA2 (,))) $ (arr extractValue &&& arr extractDate) . returnA <$> latest (res ^. responseBody . _Array)
    where
        extractDate = (^? key "date" . _String . unpacked . _LocalTime)
        extractValue = (^? key "value" . _Double)
        latest = maximumByOf traverse (compare `on` (^? key "date" . _String))

buildText :: Double -> LocalTime -> T.Text
buildText weight date = T.pack $ mconcat [
    "最終更新日は"
  , replace '-' '/' . showGregorian . localDay $ date
  , "の"
  , show (weight / 1000)
  , "kgです。がんばって太りましょう #ひにひにげそげそ #bot"
  ]

getTWInfo :: IO TWInfo
getTWInfo = do
    [consumerKey, consumerSecret, accessToken, accessTokenSecret] <- traverse ((fmap S8.pack) . getEnv) [
        "HINIHINI_GESOGESO_CONSUMER_KEY"
      , "HINIHINI_GESOGESO_CONSUMER_SECRET"
      , "HINIHINI_GESOGESO_ACCESS_TOKEN"
      , "HINIHINI_GESOGESO_ACCESS_TOKEN_SECRET"
      ]
    let oauth = twitterOAuth
            { oauthConsumerKey = consumerKey
            , oauthConsumerSecret = consumerSecret
            }
        cred = Credential
            [ ("oauth_token", accessToken)
            , ("oauth_token_secret", accessTokenSecret)
            ]
    pure $ setCredential oauth cred def

tweet :: T.Text -> IO ()
tweet text = do
    twInfo <- getTWInfo
    mgr <- newManager tlsManagerSettings
    res <- call twInfo mgr (update text)
    pure ()
