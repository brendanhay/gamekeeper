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
    getBody
  ) where

import Data.ByteString.Char8        (ByteString, pack)
import Network.HTTP.Conduit  hiding (queryString, path)
import Text.Regex                   (matchRegex, mkRegex)
import GameKeeper.Console           (displayInfo)

import qualified Data.ByteString.Lazy as L

data Uri = Uri ByteString ByteString String

--
-- API
--

getBody :: String -> IO L.ByteString
getBody uri = do
    displayInfo "GET" uri
    withManager $ \manager -> do
        Response _ _ _ body <- httpLbs (mkRequest uri) manager
        return body

--
-- Private
--

mkRequest :: String -> Request m
mkRequest uri = case parseUrl path of
    Just req -> applyBasicAuth user pass req
    Nothing  -> error $ "Invalid Request: " ++ uri
  where
    (Uri user pass path) = parseUri uri

parseUri :: String -> Uri
parseUri str = case matchRegex (mkRegex "^(.+://)(.+):(.+)@(.+)$") str of
        Just [scheme, user, pass, path] ->
            Uri (pack user) (pack pass) $ scheme ++ path
        _ ->
            error $ "Invalid URI: " ++ str
