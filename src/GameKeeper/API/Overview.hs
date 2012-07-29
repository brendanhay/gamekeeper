-- |
-- Module      : GameKeeper.API.Overview
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module GameKeeper.API.Overview (
    Overview
  , showOverview
  ) where

import Prelude             hiding (show)
import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson          (decode')
import Data.Aeson.Types
import Data.Maybe          (fromMaybe, fromJust)
import GameKeeper.Http
import GameKeeper.Metric

data Count = Count
    { total   :: Double
    , ready   :: Double
    , unacked :: Double
    } deriving (Show)

data Rate = Rate
    { publish   :: Double
    , deliver   :: Double
    , redeliver :: Double
    , confirm   :: Double
    , ack       :: Double
    } deriving (Show)

data Overview = Overview Count Rate deriving (Show)

instance FromJSON Count where
    parseJSON (Object o) = Count
        <$> o .: "messages"
        <*> o .: "messages_ready"
        <*> o .: "messages_unacknowledged"
    parseJSON _ = empty

instance FromJSON Rate where
    parseJSON (Object o) = Rate
        <$> rate "publish_details"
        <*> rate "deliver_get_details"
        <*> rate "redeliver_details"
        <*> rate "confirm_details"
        <*> rate "ack_details"
      where
        rate k = do
            v <- o .:? k
            return . fromMaybe 0 $ v >>= parseMaybe (.: "rate")
    parseJSON _ = empty

instance FromJSON Overview where
    parseJSON (Object o) = Overview
        <$> o .: "queue_totals"
        <*> o .: "message_stats"
    parseJSON _ = empty

instance Measurable Count where
    measure Count{..} =
        [ Gauge group "message.total" total
        , Gauge group "message.ready" ready
        , Gauge group "message.unacked" unacked
        ]

instance Measurable Rate where
    measure Rate{..} =
        [ Gauge group "rate.publish" publish
        , Gauge group "rate.deliver" deliver
        , Gauge group "rate.redeliver" redeliver
        , Gauge group "rate.confirm" confirm
        , Gauge group "rate.ack" ack
        ]

instance Measurable Overview where
    measure (Overview c r) = measure c ++ measure r

--
-- API
--

showOverview :: Uri -> IO Overview
showOverview uri = do
    body <- getBody uri { uriPath = "api/overview" }
    return $ fromJust (decode' body :: Maybe Overview)
