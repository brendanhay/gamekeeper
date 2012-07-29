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
    -- * Exported Types
      Exchange

    -- * HTTP Requests
    , listExchanges
    ) where

import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson          (decode')
import Data.Aeson.Types
import Data.Maybe          (fromMaybe)
import Data.Vector         (Vector)
import GameKeeper.Http
import GameKeeper.Metric

import qualified Data.ByteString.Char8 as BS

data Exchange = Exchange
    { name :: BS.ByteString
    , rate :: Double
    } deriving (Show)

instance FromJSON Exchange where
    parseJSON (Object o) = Exchange
        <$> do name <- o .: "name"
               return $ if BS.null name then "default" else name
        <*> do stats <- o .:? "message_stats_in"
               return . fromMaybe 0 $ stats >>= parseMaybe rate
      where
        rate v = (v .: "publish_details") >>= (.: "rate")
    parseJSON _ = empty

instance Measurable Exchange where
    measure Exchange{..} =
        [ Gauge group (bucket "exchange.rate" $ esc name) rate
        ]

--
-- API
--

listExchanges :: Uri -> IO [Exchange]
listExchanges uri = getList uri "api/exchanges" query decode
  where
    decode b = decode' b :: Maybe (Vector Exchange)
    query    = "?columns=name,message_stats_in.publish_details.rate"
