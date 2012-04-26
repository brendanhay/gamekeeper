{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module Monitor.RabbitMQ.Overview (
    Overview(..)
  ) where

import GHC.Generics              (Generic)
import Data.Data                 (Data, Typeable)
import Data.Aeson                (FromJSON)
import Monitor.RabbitMQ.Retrieve (Retrieve)

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
instance Retrieve Overview
