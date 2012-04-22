module Monitor.Http
    ( getBody
    ) where

import Network.HTTP.Conduit
import Data.Conduit
import Data.Maybe (fromJust)
import Monitor.Uri

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as L

getBody :: Uri -> IO L.ByteString
getBody uri = do
    print uri
    let req = request uri
    withManager $ \manager -> do
        Response _ _ _ body <- httpLbs req manager
        return body

request :: Uri -> (Request m)
request (Uri user pass path) = applyBasicAuth user pass $ fromJust $ parseUrl path
