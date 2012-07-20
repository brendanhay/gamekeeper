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
import Data.Vector           (Vector, toList)
import GameKeeper.Http

data Connection = Connection
    { name     :: String
    , user     :: String
    , received :: POSIXTime
    , sent     :: POSIXTime
    } deriving (Show)

instance FromJSON Connection where
    parseJSON (Object o) = Connection
        <$> o .: "name"
        <*> o .: "user"
        <*> ((o .: "recv_oct_details") >>= (.: "last_event"))
        <*> ((o .: "send_oct_details") >>= (.: "last_event"))
    parseJSON _ = empty

instance FromJSON POSIXTime where
    parseJSON json = liftM msToSeconds (parseJSON json)

--
-- API
--

list :: String -> IO [Connection]
list uri = do
    body <- getBody $ concat [uri, "api/connections", qs]
    return $ case (decode' body :: Maybe (Vector Connection)) of
        Just v  -> toList v
        Nothing -> []
  where
    qs = "?columns=name,user,recv_oct_details.last_event,send_oct_details.last_event"

idle :: String -> Integer -> IO [Connection]
idle uri days = do
    time <- getPOSIXTime
    liftM (filter (stale days time)) (list uri)

--
-- Private
--

stale :: Integer -> POSIXTime -> Connection -> Bool
stale days time Connection{..} = all diff [received, sent]
  where
    diff n = time >= n + 86400 * fromIntegral days

msToSeconds :: Integer -> POSIXTime
msToSeconds = realToFrac . (/ (1000 :: Double)) . fromIntegral
