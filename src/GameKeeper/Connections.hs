{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

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
  ) where

import Prelude hiding      (product)
import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson          (FromJSON, decode')
import Data.Aeson.Types
import Data.Data
import Data.Maybe          (fromMaybe)
import Data.Vector         (Vector, toList)
import GameKeeper.Http

data Connection = Connection
    { name      :: String
    , user      :: String
    , client    :: Maybe String
    , last_recv :: Integer
    , last_send :: Integer
    } deriving (Show, Data, Typeable)

instance FromJSON Connection where
    parseJSON (Object v) = Connection <$> -- fmap
        v .: "name" <*> -- apply
        v .: "user" <*>
        ((v .: "client_properties") >>= (.:? "product")) <*>
        ((v .: "recv_oct_details") >>= (.: "last_event")) <*>
        ((v .: "send_oct_details") >>= (.: "last_event"))
    parseJSON _ = empty

--
-- API
--

connections :: String -> IO [Connection]
connections base = do
    body <- getBody $ uri
    return $ case (decode' body :: Maybe (Vector Connection)) of
        Just v  -> toList v
        Nothing -> []
  where
    uri = concat [base, "api/connections", qs]
    qs  = "?columns=name,user,recv_oct_details.last_event,send_oct_details.last_event,client_properties"

--
-- Private
--
