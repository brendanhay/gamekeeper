{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, RecordWildCards,
    TypeSynonymInstances #-}

-- |
-- Module      : GameKeeper.Connections
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module GameKeeper.Connections (
    Connection
  , connections
  , stale
  ) where

import Prelude hiding      (product)
import Control.Applicative ((<$>), (<*>), empty)
import Control.Monad       (liftM, filterM)
import Data.Aeson          (decode')
import Data.Aeson.Types
import Data.Data
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Vector         (Vector, toList)
import GameKeeper.Http

data Connection = Connection
    { name      :: String
    , user      :: String
    , client    :: Maybe String
    , last_recv :: NominalDiffTime
    , last_send :: NominalDiffTime
    } deriving (Show, Data, Typeable)

instance FromJSON Connection where
    parseJSON (Object v) = Connection <$>
        v .: "name" <*>
        v .: "user" <*>
        ((v .: "client_properties") >>= (.:? "product")) <*>
        ((v .: "recv_oct_details") >>= (.: "last_event")) <*>
        ((v .: "send_oct_details") >>= (.: "last_event"))
    parseJSON _ = empty

instance FromJSON NominalDiffTime where
    parseJSON json = liftM msToSeconds (parseJSON json)

--
-- API
--

connections :: String -> IO [Connection]
connections base = do
    body <- getBody $ concat [base, "api/connections", qs]
    print body
    return $ case (decode' body :: Maybe (Vector Connection)) of
        Just v  -> toList v
        Nothing -> []
  where
    qs = "?columns=name,user,recv_oct_details.last_event,send_oct_details.last_event,client_properties"

stale :: String -> Integer -> IO [Connection]
stale base days = connections base >>= filterM (idle days)

--
-- Private
--

idle :: Integer -> Connection -> IO Bool
idle days Connection{..} =
    liftM (check last_recv last_send) getPOSIXTime
  where
    check x y c = diff c x && diff c y
    diff c n    = c >= addDays n days

msToSeconds :: Integer -> NominalDiffTime
msToSeconds = realToFrac . (/ 1000) . fromIntegral

addDays :: NominalDiffTime -> Integer -> NominalDiffTime
addDays time days = time + 86400 * fromIntegral days
