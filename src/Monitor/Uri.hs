{-# LANGUAGE OverloadedStrings #-}

module Monitor.Uri
    ( Uri(..)
    , getEnvUri
    , addPath
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.List (delete, takeWhile)
import Data.Maybe (fromJust)
import Data.ByteString.UTF8
import System.Environment (getEnv)
import Network.URI hiding (scheme)

data Uri = Uri
    { uriUser :: ByteString
    , uriPass :: ByteString
    , uriHost :: String
    } deriving (Show)

addPath :: String -> String -> String
addPath left right = concat [left, "/", right]

getEnvUri :: String -> IO Uri
getEnvUri env = do
    var <- getEnv env
    return $ conv $ fromJust $ parseURI var

conv :: URI -> Uri
conv var = Uri
    { uriUser = fromString $ user var
    , uriPass = fromString $ pass var
    , uriHost = host var
    }

auth :: URI -> String
auth = uriUserInfo . fromJust . uriAuthority

user :: URI -> String
user = takeWhile (/= ':') . auth

pass :: URI -> String
pass = delete '@' . tail . dropWhile (/= ':') . auth

scheme :: URI -> String
scheme u  = case uriScheme u of
    _ -> "http://"

host :: URI -> String
host u = case fromJust $ uriAuthority u of
    (URIAuth _ host port) -> concat [scheme u, host, port]
