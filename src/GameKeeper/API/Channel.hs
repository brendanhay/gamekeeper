-- |
-- Module      : GameKeeper.API.Channel
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module GameKeeper.API.Channel (
    -- * Exported Types
      Channel

    -- * HTTP Requests
    , listChannels
    ) where

import Data.Aeson       (decode')
import Data.Aeson.Types
import Data.Maybe       (isJust)
import Data.Vector      (Vector)
import GameKeeper.Http
import GameKeeper.Metric

data Channel
    = Publisher
    | Consumer
    | Duplex
    | Inactive
      deriving (Show)

data Count = Count
    { publisher :: Double
    , consumer  :: Double
    , duplex    :: Double
    , inactive  :: Double
    }

instance FromJSON Channel where
    parseJSON (Array _)  = return Inactive
    parseJSON (Object o) = do
        x <- o .:? "message_stats"
        case x of
          Just (Object m) -> do
                 p <- m .:? "publish" :: Parser (Maybe Int)
                 c <- m .:? "deliver" :: Parser (Maybe Int)
                 return $ channel (isJust p) (isJust c)
          _ -> return Inactive
    parseJSON o = error $ show o

instance Measurable [Channel] where
    measure xs =
        [ Gauge group "channel.total" $ len xs
        , Gauge group "channel.publisher" publisher
        , Gauge group "channel.consumer" consumer
        , Gauge group "channel.duplex" duplex
        , Gauge group "channel.inactive" inactive
        ]
      where
        Count{..} = foldl increment (Count 0 0 0 0) xs

--
-- API
--

listChannels :: Uri -> IO [Channel]
listChannels uri = getList uri "api/channels" query decode
  where
    decode b = decode' b :: Maybe (Vector Channel)
    query    = "?columns=message_stats.publish,message_stats.deliver"

--
-- Private
--

channel :: Bool -> Bool -> Channel
channel True False = Publisher
channel False True = Consumer
channel True True  = Duplex
channel _ _        = Inactive

increment :: Count -> Channel -> Count
increment c@Count{..} ctor = case ctor of
    Publisher -> c { publisher = publisher + 1 }
    Consumer  -> c { consumer  = consumer + 1 }
    Duplex    -> c { duplex    = duplex + 1 }
    Inactive  -> c { inactive  = inactive + 1 }