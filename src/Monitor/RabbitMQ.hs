{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Monitor.RabbitMQ (
    overview
  ) where

import GHC.Generics         (Generic)
import Data.List            (concat, intersperse)
import Data.Data
import Data.ByteString.Lazy (ByteString)
import Data.Aeson           (FromJSON, ToJSON, decode)
import Monitor.Uri          (Uri, getEnvUri, joinUri)
import Monitor.Http         (getBody)
import Monitor.RabbitMQ.Overview

overview :: IO (Maybe Overview)
overview = do
    body <- raw $ "/api/overview?columns=" ++ (columns (undefined :: Overview))
    return (decode body :: Maybe Overview)

raw :: String -> IO ByteString
raw path = do
    uri <- getEnvUri "RABBITMQ_MGMT_URI"
    getBody $ joinUri uri path

columns :: (Data a) => a -> String
columns = join . map constrFields . dataTypeConstrs . dataTypeOf

join :: [[String]] -> String
join = concat . intersperse "," . concat