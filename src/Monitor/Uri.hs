module Monitor.Uri (
    Uri(..)
  , getEnvUri
  , joinUri
  ) where

import System.Environment    (getEnv)
import Text.Regex            (Regex(..), matchRegex, mkRegex)
import Data.ByteString.Char8 (ByteString, pack)

data Uri = Uri ByteString ByteString String deriving (Show)

getEnvUri :: String -> IO Uri
getEnvUri env = do
    var <- getEnv env
    return $ conv $ matchRegex uri var

joinUri :: Uri -> String -> Uri
joinUri (Uri user pass path) path' = Uri user pass (path ++ path')

uri :: Regex
uri = mkRegex "^http://(.+):(.+)@(.+)$"

conv :: Maybe [String] -> Uri
conv (Just [user, pass, path]) = Uri (pack user) (pack pass) (absolute path)
conv Nothing = error "Invalid Uri, expected: http://<user>:<pass>@<host+port+path>"

absolute :: String -> String
absolute = ("http://" ++) . reverse . dropWhile (== '/') . reverse
