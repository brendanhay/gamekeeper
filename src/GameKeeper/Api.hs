-- |
-- Module      : GameKeeper.API
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module GameKeeper.Api (
    Overview
  , Count
  , overview
  , counts
  ) where

import GHC.Generics  (Generic)
import Control.Monad (mzero)
import Data.Data
import Data.Char     (toLower)
import Data.Maybe    (fromJust)
import Data.Aeson    (FromJSON, decode')
import Data.Aeson.Types
import Data.List     (concat, intersperse, intercalate)
import GameKeeper.Uri   (Uri, Param, getEnvUri, join)
import GameKeeper.Http  (getBody)

import qualified Data.Vector          as V
import qualified Data.ByteString.Lazy as L

data MessageDetails = MessageDetails
    { rate       :: Float
    , interval   :: Integer
    , last_event :: Integer
    } deriving (Show, Generic, Data, Typeable)

instance FromJSON MessageDetails

data MessageStats = MessageStats
    { confirm                :: Integer
    , confirm_details        :: MessageDetails
    , publish                :: Integer
    , publish_details        :: MessageDetails
    , ack                    :: Integer
    , ack_details            :: MessageDetails
    , deliver                :: Integer
    , deliver_details        :: MessageDetails
    , deliver_get            :: Integer
    , deliver_get_details    :: MessageDetails
    , deliver_no_ack         :: Integer
    , deliver_no_ack_details :: MessageDetails
    , redeliver              :: Integer
    , redeliver_details      :: MessageDetails
    } deriving (Show, Generic, Data, Typeable)

instance FromJSON MessageStats

data QueueTotals = QueueTotals
    { messages                :: Integer
    , messages_ready          :: Integer
    , messages_unacknowledged :: Integer
    } deriving (Show, Generic, Data, Typeable)

instance FromJSON QueueTotals

data Overview = Overview
    { message_stats      :: MessageStats
    , queue_totals       :: QueueTotals
    } deriving (Show, Generic, Data, Typeable)

instance FromJSON Overview

data Count = Count String Int deriving (Show)

instance FromJSON Count where
    parseJSON (Array vector) = return $ Count "" $ V.length vector
    parseJSON _              = mzero

--
-- API
--

overview :: IO (Maybe Overview)
overview = do
    body <- retrieve "overview" [("columns", fields (undefined :: Overview))]
    return $ (decode' body :: Maybe Overview)

counts :: IO [Count]
counts = sequence $ map count table

--
-- Private
--

retrieve :: String -> [Param] -> IO L.ByteString
retrieve res params = do
    base <- getEnvUri "RABBITMQ_MGMT_URI"
    getBody $ join base ("/api/" ++ res) params

fields :: Data a => a -> [String]
fields = concat . map constrFields . dataTypeConstrs . dataTypeOf

value :: Maybe Count -> Int
value (Just (Count _ n)) = n
value Nothing            = 0

count :: String -> IO Count
count res = do
    body <- retrieve res [("columns", [])]
    return $ Count res $ value (decode' body :: Maybe Count)

table :: [String]
table =
    [ "exchanges"
    , "queues"
    , "bindings"
    ]
