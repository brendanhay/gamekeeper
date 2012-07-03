{-# LANGUAGE OverloadedStrings #-}

module GameKeeper.Http (
    getBody
  ) where

import Network.HTTP.Conduit hiding (queryString, path)
import Text.Regex                  (matchRegex, mkRegex)
import Data.ByteString.Char8       (ByteString, pack)

import qualified Data.ByteString.Lazy as L

data Uri = Uri ByteString ByteString String

--
-- API
--

getBody :: String -> IO L.ByteString
getBody uri = do
    print uri
    withManager $ \manager -> do
        Response _ _ _ body <- httpLbs (mkRequest uri) manager
        return body

--
-- Private
--

mkRequest :: String -> Request m
mkRequest uri = case parseUrl path of
    Just req -> applyBasicAuth user pass req
    Nothing  -> error $ "Invalid Request:"
  where
    (Uri user pass path) = parseUri uri

parseUri :: String -> Uri
parseUri str = case matchRegex (mkRegex "^(.+://)(.+):(.+)@(.+)$") str of
        Just [scheme, user, pass, path] ->
            Uri (pack user) (pack pass) $ scheme ++ path
        _ ->
            error "Invalid URI"
