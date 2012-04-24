{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Monitor.RabbitMQ (
    overview
  ) where

import GHC.Generics (Generic)
import Data.Maybe   (fromJust)
import Data.Aeson   (FromJSON, ToJSON, decode)
import Monitor.Uri  (Uri, getEnvUri, join)
import Monitor.Http (getBody)

data Details = Details
    { rate       :: Float
    , interval   :: Integer
    , last_event :: Integer
    } deriving (Show, Generic)

instance FromJSON Details
instance ToJSON Details

data MessageStats = MessageStats
    { confirm         :: Integer
    , confirm_details :: Details
    } deriving (Show, Generic)

instance FromJSON MessageStats
instance ToJSON MessageStats

data Overview = Overview
    { management_version :: String
    , statistics_level   :: String
    , message_stats      :: MessageStats
    } deriving (Show, Generic)

instance FromJSON Overview
instance ToJSON Overview

overview :: IO (Maybe Overview)
overview = do
    uri <- getUri
    body <- getBody $ join uri "/api/overview"
    return (decode body :: Maybe Overview)

getUri :: IO Uri
getUri = getEnvUri "RABBITMQ_MGMT_URI"
