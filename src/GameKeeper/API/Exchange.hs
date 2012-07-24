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

import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson          (decode')
import Data.Aeson.Types
import Data.Vector         (Vector)
import GameKeeper.Http
import Network.Metric

import GameKeeper.Metric as M

import qualified Data.ByteString.Char8 as BS

data Exchange = Exchange
    { name :: BS.ByteString
    , rate :: Double
    } deriving (Show)

instance FromJSON Exchange where
    parseJSON (Object o) = Exchange
        <$> do n <- o .: "name"
               return $ if BS.null n then "default" else n
        <*> do m <- o .:? "message_stats_in"
               case m of
                   Just v  -> (v .: "publish_details") >>= (.: "rate")
                   Nothing -> return 0

    parseJSON _ = empty

instance Measurable Exchange where
    measure Exchange{..} = [Gauge group (bucket "exchange.rate" name) rate]

--
-- API
--

list :: Uri -> IO [Exchange]
list uri = getList uri { uriPath = path, uriQuery = query } decode
  where
    decode b = decode' b :: Maybe (Vector Exchange)
    path     = "api/exchanges"
    query    = "?columns=name,message_stats_in.publish_details.rate"