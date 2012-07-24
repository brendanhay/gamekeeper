-- |
-- Module      : GameKeeper.API.Exchange
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module GameKeeper.API.Exchange (
    Exchange
  , list
  ) where

import Control.Monad.IO.Class
import Control.Applicative ((<$>), (<*>), empty)
import Control.Monad       (liftM, void)
import Data.Aeson          (decode')
import Data.Aeson.Types
import Data.Vector         (Vector, toList)
import GameKeeper.Http
import Network.Metric

import GameKeeper.Metric as M

import qualified Data.ByteString.Char8 as BS

-- {
--     "message_stats_out":{
--         "publish":22304588,
--         "publish_details":{
--             "rate":572.0028101433729,
--             "interval":5693001,
--             "last_event":1343132939016
--         }
--     },
--     "message_stats_in":{
--         "publish":22304588,
--         "publish_details":{
--             "rate":572.0028101433729,
--             "interval":5693001,
--             "last_event":1343132939016
--         }
--     },
--     "name":"activities.propagate",
--     "vhost":"/",
--     "type":"topic",
--     "durable":true,
--     "auto_delete":false,
--     "internal":false,
--     "arguments":{
--     }
-- }

data Exchange = Exchange
    { name :: BS.ByteString
    , rate :: Maybe Double
    } deriving (Show)

instance FromJSON Exchange where
    parseJSON (Object o) = Exchange
        <$> o .: "name"
        -- <*> ((o .:? "message_stats_in") >>= (.: "publish_details") >>= (.: "rate"))

        -- <*> do stats <- o .:? "message_stats_in"
        --        return $ do
        --            foo <- stats
        --            ((foo .: "publish_details") >>= (.: "rate"))

        <*> do stats <- o .:? "message_stats_in"
               case stats of
                   Just v -> ((v .: "publish_details") >>= (.: "rate"))
                   Nothing -> return Nothing

    parseJSON _ = empty

instance Measurable Exchange where
    measure Exchange{..} = [Gauge group (bucket "exchange.rate" name) (val rate)]
       where
         val (Just n) = n
         val Nothing  = 0

--
-- API
--

list :: Uri -> IO [Exchange]
list uri = do
    body <- getBody uri { uriPath = "api/exchanges", uriQuery = qs }
    return $ case (decode' body :: Maybe (Vector Exchange)) of
        Just v  -> toList v
        Nothing -> []
  where
    qs = "?columns=name,message_stats_in.publish_details.rate"

--
-- Private
--

megabytes :: Double -> Double
megabytes = (!! 2) . iterate (/ 1024)
