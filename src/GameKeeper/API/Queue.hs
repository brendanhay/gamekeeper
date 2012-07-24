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
  , list
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
    , messages  :: Integer
    , consumers :: Integer
    , memory    :: Double
    } deriving (Show)

instance FromJSON Queue where
    parseJSON (Object o) = Queue
        <$> o .: "name"
        <*> o .: "messages"
        <*> o .: "consumers"
        <*> liftM megabytes (o .: "memory")
    parseJSON _ = empty

instance Measurable Queue where
    measure Queue{..} =
        [ Gauge group (bucket "queue.messages" name) (fromIntegral messages)
        , Gauge group (bucket "queue.consumers" name) (fromIntegral consumers)
        , Gauge group (bucket "queue.memory" name) memory
        ]

--
-- API
--

list :: Uri -> IO [Queue]
list uri = getList uri "api/queues" "?columns=name,messages,consumers,memory" decode
  where
    decode b = decode' b :: Maybe (Vector Queue)

--
-- Private
--

megabytes :: Double -> Double
megabytes = (!! 2) . iterate (/ 1024)
