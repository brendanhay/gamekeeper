{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : GameKeeper.API.Connection
-- Copyright   : (c) 2012-2015 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module GameKeeper.API.Connection (
    -- * Exported Types
      Connection

    -- * HTTP Requests
    , listConnections
    , deleteConnection

    -- * Filters
    , idleConnections
    ) where

import Control.Applicative   ((<$>), (<*>), empty)
import Control.Monad         (liftM)
import Data.Aeson            (decode')
import Data.Aeson.Types
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Data.Vector           (Vector)
import GameKeeper.Http
import GameKeeper.Metric

import qualified Data.ByteString.Char8 as BS

data Connection = Connection
    { name     :: BS.ByteString
    , received :: Maybe POSIXTime
    , sent     :: Maybe POSIXTime
    } deriving (Show)

instance FromJSON Connection where
    parseJSON (Object o) = Connection
        <$> o .: "name"
        <*> event "send_oct_details"
        <*> event "recv_oct_details"
      where
        event k = do
            d <- o .:? k
            return $ d >>= parseMaybe (.: "last_event")
    parseJSON _ = empty

instance FromJSON POSIXTime where
    parseJSON json = liftM msToSeconds (parseJSON json)

instance Measurable [(Bool, Connection)] where
    measure lst =
        [ Gauge group "connection.total" $ len lst
        , Gauge group "connection.idle" . len $ idle lst
        ]

--
-- API
--

listConnections :: Uri -> IO [Connection]
listConnections uri = list uri "api/connections" query decode
  where
    decode b = decode' b :: Maybe (Vector Connection)
    query    = "?columns=name,recv_oct_details.last_event,send_oct_details.last_event"

deleteConnection :: Uri -> Connection -> IO ()
deleteConnection uri Connection{..} = do
    _ <- delete uri { uriPath = BS.concat ["api/connections/", name] }
    return ()

idleConnections :: Int -> [Connection] -> IO [(Bool, Connection)]
idleConnections days lst = do
    n <- getPOSIXTime
    return [(isIdle days n conn, conn) | conn <- lst]

--
-- Private
--

-- FIXME: Purging connections should be done at the channel level
-- since the assumption a connection is idle by received/sent events
-- at the connection level doesn't seem to hold - specifically for
-- connections which don't contain a receiving channel.

isIdle :: Int -> POSIXTime -> Connection -> Bool
isIdle days time Connection{..} = all diff [received, sent]
  where
    diff (Just n) = time >= n + 86400 * fromIntegral days
    diff Nothing  = False

msToSeconds :: Integer -> POSIXTime
msToSeconds = realToFrac . (/ (1000 :: Double)) . fromIntegral
