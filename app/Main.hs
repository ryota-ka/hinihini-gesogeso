module Main where

import Lib
import Control.Applicative (liftA2)

main :: IO ()
main = do
    height <- getHeight
    x <- (liftA2 (\(weight, time) (fat, _) -> (weight, time, fat))) <$> fetchLatestWeight <*> fetchLatestFat
    case x of
        Nothing -> pure ()
        Just (weight, time, fat) -> tweet (buildText weight fat height time)
