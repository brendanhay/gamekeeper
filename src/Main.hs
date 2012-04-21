module Main where

import Data.Maybe (fromJust)
import System.Environment (getEnv)
import Network.URI (parseURI)
import Monitor.RabbitMQ (overview)

env = "RABBITMQ_MGMT_URI"

main :: IO ()
main = do
    res <- getEnv env
    -- res <- overview
    print $ fromJust $ parseURI res

