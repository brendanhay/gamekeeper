module Main where

import Monitor.RabbitMQ

main :: IO ()
main = do
    counts' <- counts
    print counts'
