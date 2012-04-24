{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Monitor.RabbitMQ (
    overview
  ) where

import GHC.Generics (Generic)
import Data.Data
import Data.Char    (toLower)
import Data.Aeson   (FromJSON, ToJSON, decode)
import Data.List    (concat, intersperse)
import Monitor.Uri  (Uri, getEnvUri, joinUri)
import Monitor.Http (getBody)

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
instance Retrievable Overview

class (FromJSON a, Data a, Typeable a) => Retrievable a where
    location :: a -> String
    location m = (root m) ++ (columns m)

    retrieve :: a -> IO (Maybe a)
    retrieve m = do
        base <- getEnvUri "RABBITMQ_MGMT_URI"
        body <- getBody $ joinUri base $ location m
        return (decode body :: (FromJSON m) => Maybe m)

overview :: IO (Maybe Overview)
overview = retrieve (undefined :: Overview)

-- Private

root :: (Typeable a) => a -> String
root = ("/api/" ++) . map toLower . show . typeOf

columns :: (Data a) => a -> String
columns = ("?columns=" ++) . join . map constrFields . dataTypeConstrs . dataTypeOf

join :: [[String]] -> String
join = concat . intersperse "," . concat
