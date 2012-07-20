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
  , stats
  ) where

import Control.Monad.IO.Class
import Control.Applicative ((<$>), (<*>), empty)
import Control.Monad       (liftM, void)
import Data.Aeson          (decode')
import Data.Aeson.Types
import Data.Vector         (Vector, toList)
import GameKeeper.Http
import Network.Metric

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

--
-- API
--

list :: String -> IO [Queue]
list uri = do
    body <- getBody $ concat [uri, "api/queues", qs]
    return $ case (decode' body :: Maybe (Vector Queue)) of
        Just v  -> toList v
        Nothing -> []
  where
    qs = "?columns=name,messages,consumers,memory"

stats :: Sink a => String -> a -> IO ()
stats uri sink = list uri >>= mapM_ (collect sink)

--
-- Private
--

megabytes :: Double -> Double
megabytes = (!! 2) . iterate (/ 1024)

collect :: Sink a => a -> Queue -> IO ()
collect sink Queue{..} = do
    p $ Gauge "rabbitmq.queue.messages" name messages
    p $ Gauge "rabbitmq.queue.memory" name memory
    p $ Gauge "rabbitmq.queue.consumers" name consumers
  where
    p :: Encodable a => Metric a -> IO ()
    p = push sink

