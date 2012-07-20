{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : GameKeeper.Api.Connections
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module GameKeeper.Api.Connections (
    Connection
  , connections
  , stale
  ) where

import Prelude hiding        (product)
import Control.Applicative   ((<$>), (<*>), empty)
import Control.Monad         (liftM, filterM)
import Data.Aeson            (decode')
import Data.Aeson.Types
import Data.Data
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Data.Vector           (Vector, toList)
import GameKeeper.Http

data Connection = Connection
    { name      :: String
    , user      :: String
    , last_recv :: POSIXTime
    , last_send :: POSIXTime
    } deriving (Show, Data, Typeable)

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

connections :: String -> IO [Connection]
connections base = do
    body <- getBody $ concat [base, "api/connections", qs]
    print body
    return $ case (decode' body :: Maybe (Vector Connection)) of
        Just v  -> toList v
        Nothing -> []
  where
    qs = "?columns=name,user,recv_oct_details.last_event,send_oct_details.last_event"

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
    diff c n    = c >= n + 86400 * fromIntegral days

msToSeconds :: Integer -> POSIXTime
msToSeconds = realToFrac . (/ (1000 :: Double)) . fromIntegral
