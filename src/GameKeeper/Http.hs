module GameKeeper.Http (
    getBody
  ) where

import Network.HTTP.Conduit
import GameKeeper.Uri (Uri(..))

import qualified Data.ByteString.Lazy as L

-- API

getBody :: Uri -> IO L.ByteString
getBody uri = do
    print uri
    let req = request uri
    withManager $ \manager -> do
        Response _ _ _ body <- httpLbs req manager
        return body

-- Private

request :: Uri -> Request m
request (Uri user pass path) = case parseUrl path of
    Just req -> applyBasicAuth user pass req
    Nothing  -> error "Invalid Request"
