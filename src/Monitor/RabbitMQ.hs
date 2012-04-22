{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Monitor.RabbitMQ
    ( overview
    ) where

import GHC.Generics (Generic)
import Data.Maybe (fromJust)
import Data.Aeson (FromJSON, ToJSON, decode)
import Monitor.Uri (Uri, getEnvUri)
import Monitor.Http (getBody)

data Overview = Overview
    { management_version :: String
    , statistics_level :: String
    } deriving (Show, Generic)

instance FromJSON Overview
instance ToJSON Overview

overview :: IO (Maybe Overview)
overview = do
    uri <- getEnvUri "RABBITMQ_MGMT_URI"
    body <- getBody uri "/api/overview"
    return (decode body :: Maybe Overview)
