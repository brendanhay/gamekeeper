{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Monitor.RabbitMQ
    ( overview
    ) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, decode)
import Monitor.Http (request)

data Overview = Overview
    { management_version :: String
    , statistics_level :: String
    } deriving (Show, Generic)

instance FromJSON Overview
instance ToJSON Overview

overview :: IO (Maybe Overview)
overview = do
    body <- request "localhost:55672" "guest" "guest"
    return (decode body :: Maybe Overview)
