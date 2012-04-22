module Monitor.Http
    ( getBody
    ) where

import Network.HTTP.Conduit
import Data.Conduit
import Data.Maybe (fromJust)
import Monitor.Uri

import qualified Data.ByteString.Lazy as L

getBody :: Uri -> String -> IO L.ByteString
getBody uri path = do
    let req = request uri path
    withManager $ \manager -> do
        Response _ _ _ body <- httpLbs req manager
        return body

request :: Uri -> String -> (Request m)
request (Uri user pass host) path = applyBasicAuth user pass $ addPath host path

addPath :: String -> String -> (Request m)
addPath host path = fromJust $ parseUrl $ concat [host, "/", path]
