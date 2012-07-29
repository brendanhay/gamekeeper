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
    Queue
  , listQueues
  , idleQueues
  ) where

import Control.Applicative ((<$>), (<*>), empty)
import Control.Monad       (liftM)
import Data.Aeson          (decode')
import Data.Aeson.Types
import Data.Vector         (Vector)
import GameKeeper.Http
import GameKeeper.Metric

import qualified Data.ByteString.Char8 as BS

data Queue = Queue
    { name      :: BS.ByteString
    , messages  :: Double
    , consumers :: Double
    , memory    :: Double
    } deriving (Show)

instance FromJSON Queue where
    parseJSON (Object o) = Queue
        <$> o .: "name"
        <*> o .: "messages"
        <*> o .: "consumers"
        <*> liftM megabytes (o .: "memory")
    parseJSON _ = empty

instance Measurable [(Bool, Queue)] where
    measure lst =
        [ Gauge group "queue.total" $ len lst
        , Gauge group "queue.idle" $ idle lst
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

listQueues :: Uri -> IO [Queue]
listQueues uri = getList uri "api/queues" query decode
  where
    decode b = decode' b :: Maybe (Vector Queue)
    query    = "?columns=name,messages,consumers,memory"

idleQueues :: [Queue] -> [(Bool, Queue)]
idleQueues lst = zip (map (\Queue{..} -> consumers == 0) lst) lst

--
-- Private
--

megabytes :: Double -> Double
megabytes = (!! 2) . iterate (/ 1024)
