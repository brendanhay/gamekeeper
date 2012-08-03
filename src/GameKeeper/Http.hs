-- |
-- Module      : GameKeeper.Http
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module GameKeeper.Http (
    -- * Exported Types
      Uri(..)

    -- * Functions
    , parseUri
    , list
    , get
    , delete
    ) where

import Text.Printf                 (printf)
import Control.Monad.IO.Class      (liftIO)
import Data.Data                   (Data, Typeable)
import Data.Maybe                  (fromJust)
import Data.Vector                 (Vector, toList)
import Network.HTTP.Conduit hiding (queryString, path)
import Network.HTTP.Types.Method
import GameKeeper.Logger

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as BL
import qualified Network.URI           as U

data Uri = Uri
    { uriScheme  :: BS.ByteString
    , uriHost    :: BS.ByteString
    , uriPort    :: Int
    , uriUser    :: BS.ByteString
    , uriPass    :: BS.ByteString
    , uriPath    :: BS.ByteString
    , uriQuery   :: BS.ByteString
    , uriFrag    :: BS.ByteString
    } deriving (Data, Typeable, Show)

--
-- API
--

parseUri :: String -> Uri
parseUri = conv . U.parseURI

list :: Uri
     -> BS.ByteString
     -> BS.ByteString
     -> (BL.ByteString -> Maybe (Vector a))
     -> IO [a]
list uri path query decode = do
    body <- get uri { uriPath = path, uriQuery = query }
    return $ case decode body of
        Just v  -> toList v
        Nothing -> []

get :: Uri -> IO BL.ByteString
get = request "GET"

delete :: Uri -> IO BL.ByteString
delete = request "DELETE"

--
-- Private
--

request :: Method -> Uri -> IO BL.ByteString
request method uri = withManager $ \manager -> do
        Response _ _ _ body <- httpLbs req { method = method } manager
        liftIO . logInfo $ concat ["[", show method, "] -> ", abspath uri]
        liftIO . logInfo $ concat
            [ "["
            , printf "%.2f" $ kb body
            , " kB"
            , "] <- "
            , abspath uri
            ]
        return body
  where
    req     = prepare uri
    kb bstr = (/ 1024) . fromIntegral $ BL.length bstr :: Float

prepare :: Uri -> Request m
prepare uri@Uri{..} = case parseUrl $ abspath uri of
    Just req -> applyBasicAuth uriUser uriPass req
    Nothing  -> error $ "Invalid Request: " ++ abspath uri

abspath :: Uri -> String
abspath Uri{..} = BS.unpack $ BS.concat uri
  where
    uri  = [uriScheme, "//", uriHost, ":", port, "/", uriPath, uriQuery, uriFrag]
    port = BS.pack $ show uriPort

conv :: Maybe U.URI -> Uri
conv Nothing          = error "Invalid Uri"
conv (Just U.URI{..}) = Uri
    { uriScheme  = BS.pack uriScheme
    , uriHost    = BS.pack $ U.uriRegName auth
    , uriPort    = read port :: Int
    , uriUser    = BS.pack user
    , uriPass    = BS.pack $ trim '@' pass
    , uriPath    = BS.pack uriPath
    , uriQuery   = BS.pack uriQuery
    , uriFrag    = BS.pack uriFragment
    }
  where
    auth         = fromJust uriAuthority
    [user, pass] = split ':' $ U.uriUserInfo auth
    port         = trim ':' $ U.uriPort auth

split :: Char -> String -> [String]
split delim s | [] <- rest = [token]
              | otherwise  = token : split delim (tail rest)
  where
    (token, rest) = span (/= delim) s

trim :: Char -> String -> String
trim delim = f . f
  where
    f = reverse . dropWhile (== delim)