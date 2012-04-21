module Monitor.Http
    ( request
    ) where

import Network.HTTP.Conduit
import Data.Conduit
import Data.Maybe
import Data.ByteString.Internal

import qualified Data.ByteString.Lazy as L

instance Show (Request m) where
    show req = concat [show $ host req, ":", show $ port req, "\n", show $ requestHeaders req]

request :: String -> ByteString -> ByteString -> IO L.ByteString
request url user pass = do
    let req = applyBasicAuth user pass $ fromJust $ parseUrl url
    print req
    withManager $ \manager -> do
        Response _ _ _ body <- httpLbs req manager
        return body
