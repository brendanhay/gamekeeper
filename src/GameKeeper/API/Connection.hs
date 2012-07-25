{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : GameKeeper.API.Connection
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module GameKeeper.API.Connection (
    Connection
  , list
  , idle
  ) where

import Control.Applicative   ((<$>), (<*>), empty)
import Control.Monad         (liftM)
import Data.Aeson            (decode')
import Data.Aeson.Types
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Data.Vector           (Vector)
import Network.Metric
import GameKeeper.Http
import GameKeeper.Metric

import qualified Data.ByteString.Char8 as BS

data Connection = Connection
    { name     :: BS.ByteString
    , user     :: BS.ByteString
    , received :: Maybe POSIXTime
    , sent     :: Maybe POSIXTime
    } deriving (Show)

instance FromJSON Connection where
    parseJSON (Object o) = Connection
        <$> o .: "name"
        <*> o .: "user"
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
    measure xs =
        [ Gauge group "connection.total" $ len xs
        , Gauge group "connection.stale" . len $ filter ((== True) . fst) xs
        ]
      where
        len l = fromIntegral $ length l :: Double

--
-- API
--

list :: Uri -> IO [Connection]
list uri = getList uri "api/connections" query decode
  where
    decode b = decode' b :: Maybe (Vector Connection)
    query    = "?columns=name,user,recv_oct_details.last_event,send_oct_details.last_event"

idle :: Int -> [Connection] -> IO [(Bool, Connection)]
idle days xs = do
    n <- getPOSIXTime
    return [(stale days n conn, conn) | conn <- xs]

--
-- Private
--

stale :: Int -> POSIXTime -> Connection -> Bool
stale days time Connection{..} = all diff [received, sent]
  where
    diff (Just n) = time >= n + 86400 * fromIntegral days
    diff Nothing  = False

msToSeconds :: Integer -> POSIXTime
msToSeconds = realToFrac . (/ (1000 :: Double)) . fromIntegral
