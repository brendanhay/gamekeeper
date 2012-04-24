{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Monitor.RabbitMQ.Retrievable (
    Retrievable(..)
  ) where

import GHC.Generics (Generic)
import Data.List    (concat, intersperse)
import Data.Data
import Data.Char    (toLower)
import Data.Aeson   (FromJSON, decode)
import Monitor.Uri  (Uri, getEnvUri, joinUri)
import Monitor.Http (getBody)

class (FromJSON a, Data a, Typeable a) => Retrievable a where
    location :: a -> String
    location m = (root m) ++ (columns m)

    retrieve :: a -> IO (Maybe a)
    retrieve m = do
        base <- getEnvUri "RABBITMQ_MGMT_URI"
        body <- getBody $ joinUri base $ location m
        return (decode body :: (FromJSON m) => Maybe m)

root :: (Typeable a) => a -> String
root = ("/api/" ++) . map toLower . show . typeOf

columns :: (Data a) => a -> String
columns = ("?columns=" ++) . join . map constrFields . dataTypeConstrs . dataTypeOf

join :: [[String]] -> String
join = concat . intersperse "," . concat
