module Main where

import Lib

main :: IO ()
main = fetchLatestWeight >>= maybe (pure ()) (tweet . uncurry buildText)
