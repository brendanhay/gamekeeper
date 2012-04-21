{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getEnv)
import Monitor.RabbitMQ (overview)

main :: IO ()
main = do
    res <- getEnv "RABBITMQ_MGMT_URI"
    -- res <- overview
    print res

