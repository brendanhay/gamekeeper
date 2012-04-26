module Main where

import Data.Maybe       (fromJust)
import Monitor.RabbitMQ.Retrieve (retrieve)

import qualified Monitor.RabbitMQ.Overview as O
import qualified Monitor.RabbitMQ.Channel as C

main :: IO ()
main = do
    res <- retrieve (undefined :: C.Channels)
    print $ fromJust res


-- Be more specific about what values to send
-- setup graphite (no statsd) integration

-- focus on just counting, or retrieving specific values and making the serialization
-- on the rabbit side as minimal as possible (use ?columns=)

-- Overview = done (can't use columns)

-- Channels Int
-- Connections Int
-- Bindings Int
-- Exchanges Int

-- Queue {
--   name :: String
--   messages_ready :: Int
--   consumers :: Int
-- }
-- Queues [Queue]
