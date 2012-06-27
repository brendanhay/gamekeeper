module GameKeeper.Uri (
    Uri(..)
  , Param(..)
  , getEnvUri
  , join
  ) where

import System.Environment    (getEnv)
import Text.Regex            (Regex(..), matchRegex, mkRegex)
import Data.List             (concat, intercalate)
import Data.ByteString.Char8 (ByteString, pack)

data Param = String [String]

data Uri = Uri ByteString ByteString String deriving (Show)

-- API

getEnvUri :: String -> IO Uri
getEnvUri env = do
    var <- getEnv env
    return $ conv $ matchRegex uri var

join :: Uri -> String -> [Param] -> Uri
join (Uri x y z) path params = Uri x y $ concat [z, path, conj params]

-- Private

conj :: [Param] -> String
conj = ('?' :) . intercalate "&" . map flat

flat :: Param -> String
flat (k, vs) = concat [k, "=", intercalate "," vs]

uri :: Regex
uri = mkRegex "^http://(.+):(.+)@(.+)$"

conv :: Maybe [String] -> Uri
conv (Just [user, pass, path]) = Uri (pack user) (pack pass) (absolute path)
conv Nothing = error "Invalid Uri, expected: http://<user>:<pass>@<host+port+path>"

absolute :: String -> String
absolute = ("http://" ++) . reverse . dropWhile (== '/') . reverse
