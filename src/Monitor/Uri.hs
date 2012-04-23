{-# LANGUAGE OverloadedStrings #-}

module Monitor.Uri (
    Uri(..)
  , getEnvUri
  , addPath
  ) where

import System.Environment (getEnv)
import Network.HTTP.Types (Ascii)
import Network.URI hiding (scheme, uriPath)
import Data.List          (delete, takeWhile)
import Data.Maybe         (fromJust)
import Text.Regex

import qualified Data.ByteString.Char8 as S

data Uri = Uri
    { uriUser :: Ascii
    , uriPass :: Ascii
    , uriPath :: String
    } deriving (Show)

credentials :: Uri -> String
credentials (Uri x y _) = S.unpack $ S.concat [x, S.cons ':' y]

getEnvUri :: String -> IO Uri
getEnvUri env = do
    var <- getEnv env
    return $ conv $ fromJust $ parseURI var

addPath :: Uri -> String -> Uri
addPath (Uri x y z) path = Uri x y $ concat [z, path]

conv :: URI -> Uri
conv var = Uri
    { uriUser = S.pack $ user var
    , uriPass = S.pack $ pass var
    , uriPath = host var
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
