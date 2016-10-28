module Main where

import Lib

main :: IO ()
main = fetchLatestWeight >>= maybe (pure ()) (\(weight, date) -> tweet (buildText weight date))
