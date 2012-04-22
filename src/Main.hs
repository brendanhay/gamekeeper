module Main where

import Monitor.RabbitMQ (overview)

main :: IO ()
main = do
    o <- overview
    print o