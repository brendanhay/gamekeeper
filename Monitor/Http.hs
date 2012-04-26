module Monitor.Http (
    getBody
  ) where

import Network.HTTP.Conduit
import Data.Maybe  (fromJust)
import Monitor.Uri (Uri(..))

import qualified Data.ByteString.Lazy as L

getBody :: Uri -> IO L.ByteString
getBody uri = do
    print uri
    let req = request uri
    withManager $ \manager -> do
        Response _ _ _ body <- httpLbs req manager
        return body

request :: Uri -> (Request m)
request (Uri user pass path) = case parseUrl path of
    Just req -> applyBasicAuth user pass req
    Nothing  -> error "Invalid Request"