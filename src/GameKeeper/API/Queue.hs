-- |
-- Module      : GameKeeper.API.Queue
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module GameKeeper.API.Queue (
    -- * Exported Types
      Queue(messages, memory)

    -- * HTTP Requests
    , showQueue
    , listQueues
    , deleteQueue

    -- * Filters
    , idleQueues
    , unusedQueues
  ) where

import Control.Applicative ((<$>), (<*>), empty)
import Control.Monad       (liftM)
import Data.Aeson          (decode')
import Data.Aeson.Types
import Data.Maybe          (fromJust)
import Data.Vector         (Vector)
import GameKeeper.Http
import GameKeeper.Metric

import qualified Data.ByteString.Char8 as BS

data Queue = Queue
    { name      :: BS.ByteString
    , vhost     :: BS.ByteString
    , messages  :: Double
    , consumers :: Double
    , memory    :: Double
    } deriving (Show)

instance FromJSON Queue where
    parseJSON (Object o) = Queue
        <$> o .: "name"
        <*> o .: "vhost"
        <*> o .: "messages"
        <*> o .: "consumers"
        <*> liftM megabytes (o .: "memory")
    parseJSON _ = empty

instance Measurable [(Bool, Queue)] where
    measure lst =
        [ Gauge group "queue.total" $ len lst
        , Gauge group "queue.idle" . len $ idle lst
        ]

instance Measurable Queue where
    measure Queue{..} =
        [ Gauge group (bucket "queue.messages" $ esc name) messages
        , Gauge group (bucket "queue.consumers" $ esc name) consumers
        , Gauge group (bucket "queue.memory" $ esc name) memory
        ]

--
-- API
--

showQueue :: Uri -> String -> IO Queue
showQueue uri name = do
    body <- get uri { uriPath = path, uriQuery = query }
    return $ fromJust (decode' body :: Maybe Queue)
  where
    path  = BS.concat ["api/queues/", enc "/", "/", BS.pack name]
    query = "?columns=name,vhost,messages,consumers,memory"

listQueues :: Uri -> IO [Queue]
listQueues uri = list uri "api/queues" query decode
  where
    decode b = decode' b :: Maybe (Vector Queue)
    query    = "?columns=name,vhost,messages,consumers,memory"

deleteQueue :: Uri -> Queue -> IO ()
deleteQueue uri Queue{..} = do
    _ <- delete uri { uriPath = BS.concat ["api/queues/", enc vhost, "/", name] }
    return ()

idleQueues :: [Queue] -> [(Bool, Queue)]
idleQueues = filterQueues (\Queue{..} -> consumers == 0)

unusedQueues :: [Queue] -> [(Bool, Queue)]
unusedQueues = filterQueues (\Queue{..} -> consumers == 0 && messages == 0)

--
-- Private
--

filterQueues :: (Queue -> Bool) -> [Queue] -> [(Bool, Queue)]
filterQueues f lst = zip (map f lst) lst

megabytes :: Double -> Double
megabytes = (!! 2) . iterate (/ 1024)

enc :: BS.ByteString -> BS.ByteString
enc "/" = "%2f"
enc s   = s
