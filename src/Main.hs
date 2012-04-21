module Main where

import Control.Monad.Trans
import System.Environment (getEnv)
import Network.URI (parseURI)
import Monitor.RabbitMQ (overview)

env = "RABBITMQ_MGMT_URI"

uri u = case parseURI u of
    Just u  -> u
    Nothing -> error $ "Syntax error in " ++ env

main :: IO ()
main = do
    res <- getEnv env
    -- res <- overview
    print $ uri res

