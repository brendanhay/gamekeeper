module Main where

import Data.Maybe       (fromJust)
import Monitor.RabbitMQ (overview)

main :: IO ()
main = do
    res <- overview
    print $ fromJust res
