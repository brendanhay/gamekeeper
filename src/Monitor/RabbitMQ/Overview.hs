{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Monitor.RabbitMQ.Overview (
    Overview(..)
  ) where

import GHC.Generics (Generic)
import Data.Data    (Data, Typeable)
import Data.Aeson   (FromJSON, ToJSON)

data Details = Details
    { rate       :: Float
    , interval   :: Integer
    , last_event :: Integer
    } deriving (Show, Generic, Data, Typeable)

instance FromJSON Details
instance ToJSON Details

data MessageStats = MessageStats
    { confirm                :: Integer
    , confirm_details        :: Details
    , publish                :: Integer
    , publish_details        :: Details
    , ack                    :: Integer
    , ack_details            :: Details
    , deliver                :: Integer
    , deliver_details        :: Details
    , deliver_get            :: Integer
    , deliver_get_details    :: Details
    , deliver_no_ack         :: Integer
    , deliver_no_ack_details :: Details
    , redeliver              :: Integer
    , redeliver_details      :: Details
    } deriving (Show, Generic, Data, Typeable)

instance FromJSON MessageStats
instance ToJSON MessageStats

data QueueTotals = QueueTotals
    { messages                :: Integer
    , messages_ready          :: Integer
    , messages_unacknowledged :: Integer
    } deriving (Show, Generic, Data, Typeable)

instance FromJSON QueueTotals
instance ToJSON QueueTotals

data Overview = Overview
    { management_version :: String
    , statistics_level   :: String
    , message_stats      :: MessageStats
    , queue_totals       :: QueueTotals
    } deriving (Show, Generic, Data, Typeable)

instance FromJSON Overview
instance ToJSON Overview
