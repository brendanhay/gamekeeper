module Monitor.RabbitMQ.Retrieve (
    Param
  , Retrieve(..)
  ) where

import Data.Data
import Data.Char        (toLower)
import Data.Aeson       (FromJSON, decode')
import Data.List        (concat, intersperse, intercalate)
import Monitor.Uri      (Uri, Param, getEnvUri, join)
import Monitor.Http     (getBody)

class (FromJSON a, Data a, Typeable a) => Retrieve a where
    columns :: a -> [String]
    columns = intersperse "," . concat . fields

    params :: a -> [Param]
    params m = [("columns", columns m)]

    resource :: a -> String
    resource = map toLower . show . typeOf

    retrieve :: a -> IO (Maybe a)
    retrieve m = do
        u    <- uri m
        body <- getBody u
        return (decode' body :: FromJSON m => Maybe m)

fields :: Data a => a -> [[String]]
fields = map constrFields . dataTypeConstrs . dataTypeOf

uri :: Retrieve a => a -> IO Uri
uri m = do
    base <- getEnvUri "RABBITMQ_MGMT_URI"
    return $ join base (concat ["/api/", resource m]) (params m)
