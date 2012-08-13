{-# LANGUAGE NoMonomorphismRestriction #-}

-- |
-- Module      : GameKeeper.Nagios
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module GameKeeper.Nagios (
    -- * Exported Types
      Health(..)
    , Message
    , Status(..)
    , Plugin(..)
    , Check(..)

    -- * Functions
    , check
    ) where

import Prelude           hiding (catch)
import Control.Exception
import Data.List                (intercalate)

import qualified Data.ByteString.Char8 as BS
import qualified System.Exit           as E

type Title   = BS.ByteString
type Service = BS.ByteString
type Message = String

data Health  = Health Double Double deriving (Eq, Show)

data Status
    = OK
      { service :: Service
      , message :: Message
      }
      -- ^ The plugin was able to check the service and
      --   it appeared to be functioning properly
    | Warning
      { service :: Service
      , message :: Message
      }
      -- ^ The plugin was able to check the service,
      --   but it appeared to be above some "warning"
      --   threshold or did not appear to be working properly
    | Critical
      { service :: Service
      , message :: Message
      }
      -- ^ The plugin detected that either the service was
      --   not running or it was above some "critical" threshold
    | Unknown
      { service :: Service
      , message :: Message
      }
      -- ^ Invalid command line arguments were supplied
      --   to the plugin or low-level failures internal
      --   to the plugin (such as unable to fork, or open a tcp socket)
      --   that prevent it from performing the specified operation.
      --   Higher-level errors (such as name resolution errors, socket timeouts, etc)
      --   are outside of the control of plugins and should
      --   generally NOT be reported as UNKNOWN states.
      deriving (Eq, Show)

data Plugin = Plugin Title Service [Check]

data Check = Check
    { name     :: Service
    , value    :: Either SomeException Double
    , health   :: Health
    , ok       :: Double -> Message
    , warning  :: Double -> Double -> Message
    , critical :: Double -> Double -> Message
    }

--
-- API
--

check :: Plugin -> IO ()
check (Plugin title service checks) = do
    BS.putStrLn $ format acc
    mapM_ (BS.putStrLn . format) res
    E.exitWith $ code acc
  where
    f chk = status chk $ value chk
    res   = map f checks
    acc   = fold (BS.concat [title, " ", service]) res

--
-- Private
--

status :: Check -> Either SomeException Double -> Status
status Check{..} (Left e)              = Unknown name $ show e
status Check{..} (Right n) | n >= y    = Critical name $ critical n x
                           | n >= x    = Warning name $ warning n y
                           | otherwise = OK name $ ok n
  where
    (Health x y) = health

fold :: Service -> [Status] -> Status
fold serv lst | length ok == length lst = OK serv "All services healthy"
              | any' crit               = Critical serv $ text crit
              | any' unkn               = Unknown serv $ text unkn
              | any' warn               = Warning serv $ text warn
              | otherwise               = Unknown serv $ text unkn
  where
    [ok, warn, crit, unkn] = split lst
    any' = not . null
    text = intercalate ", " . map message

split :: [Status] -> [[Status]]
split lst = map f [0..3]
  where
    f n = filter ((== n) . enum) lst

enum :: Status -> Int
enum (OK       _ _) = 0
enum (Warning  _ _) = 1
enum (Critical _ _) = 2
enum (Unknown  _ _) = 3

symbol :: Status -> BS.ByteString
symbol s = case enum s of
    0 -> "OK"
    1 -> "WARNING"
    2 -> "CRITICAL"
    _ -> "UNKNOWN"

format :: Status -> BS.ByteString
format s = BS.concat [symbol s, " ", service s, " - ", BS.pack $ message s]

code :: Status -> E.ExitCode
code (OK _ _) = E.ExitSuccess
code s        = E.ExitFailure $ enum s
