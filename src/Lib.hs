{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( buildText
    , fetchLatestFat
    , fetchLatestWeight
    , getHeight
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

_LocalTime :: Prism' String LocalTime
_LocalTime = prism' show (readMaybe . replace 'T' ' ')
    where
      replace :: Eq a => a -> a -> [a] -> [a]
      replace from to = map (\x -> if x == from then to else x)

fetchLatestWeight :: IO (Maybe (Double, LocalTime))
fetchLatestWeight = do
    res <- get "https://api.ryota-ka.me/weights"
    pure . maybe Nothing (uncurry (liftA2 (,))) $ (arr extractValue &&& arr extractDate) . returnA <$> latest (res ^. responseBody . _Array)
    where
        extractDate = (^? key "date" . _String . unpacked . _LocalTime)
        extractValue = (^? key "value" . _Double)
        latest = maximumByOf traverse (compare `on` (^? key "date" . _String))

fetchLatestFat :: IO (Maybe (Double, LocalTime))
fetchLatestFat = do
    res <- get "https://api.ryota-ka.me/fats"
    pure . maybe Nothing (uncurry (liftA2 (,))) $ (arr extractValue &&& arr extractDate) . returnA <$> latest (res ^. responseBody . _Array)
    where
        extractDate = (^? key "date" . _String . unpacked . _LocalTime)
        extractValue = (^? key "value" . _Double)
        latest = maximumByOf traverse (compare `on` (^? key "date" . _String))

bmi :: Double -> Int -> Double
bmi weight height = weight * 1000 / fromIntegral (height ^ 2)

buildText :: Double -> Double -> Int -> LocalTime -> T.Text
buildText weight fat height date = T.pack $ mconcat [
    "最終更新日 : " ++ showGregorian (localDay date) ++ "\n"
  , "体重 : " ++ show (weight / 1000) ++ "kg\n"
  , "体脂肪率 : " ++ show (fat / 10000) ++ "%\n"
  , "BMI : " ++ take 5 (show (bmi weight height)) ++ "\n"
  , "がんばって太りましょう #ひにひにげそげそ"
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

getHeight :: IO Int
getHeight = read <$> getEnv "HINIHINI_GESOGESO_HEIGHT"

tweet :: T.Text -> IO ()
tweet text = do
    twInfo <- getTWInfo
    mgr <- newManager tlsManagerSettings
    res <- call twInfo mgr (update text)
    pure ()
