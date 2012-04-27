{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Monitor.RabbitMQ (
    Overview
  , Count
  , overview
  , count
  , counts
  ) where

import Control.Monad.IO.Class
import GHC.Generics  (Generic)
import Control.Monad (mzero)
import Data.Data
import Data.Char     (toLower)
import Data.Maybe    (fromJust)
import Data.Aeson    (FromJSON, decode')
import Data.Aeson.Types
import Data.List     (concat, intersperse, intercalate)
import Monitor.Uri   (Uri, Param, getEnvUri, join)
import Monitor.Http  (getBody)

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
    { management_version :: String
    , statistics_level   :: String
    , message_stats      :: MessageStats
    , queue_totals       :: QueueTotals
    } deriving (Show, Generic, Data, Typeable)

instance FromJSON Overview

data Count = Count String Int deriving (Show)

instance FromJSON Count where
    parseJSON (Array vector) = return $ Count "" $ V.length vector
    parseJSON _              = mzero

-- API

overview :: IO (Maybe Overview)
overview = do
    body <- retrieve "overview" [("columns", fields (undefined :: Overview))]
    return $ (decode' body :: Maybe Overview)

count :: String -> IO Count
count res = do
    body <- retrieve res [("columns", [])]
    return $ Count res $ case (decode' body :: Maybe Count) of
        Just (Count _ n) -> n
        Nothing          -> 0

counts :: IO [Count]
counts = sequence $ map count table

-- Private

retrieve :: String -> [Param] -> IO L.ByteString
retrieve res params = do
    base <- getEnvUri "RABBITMQ_MGMT_URI"
    getBody $ join base ("/api/" ++ res) params

table :: [String]
table =
    [ "exchanges"
    , "queues"
    , "bindings"
    ]

fields :: Data a => a -> [String]
fields = map concat . map constrFields . dataTypeConstrs . dataTypeOf