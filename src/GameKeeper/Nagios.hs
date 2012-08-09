{-# LANGUAGE MagicHash #-}

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
    , plugin
    , check
    , run
    ) where

import Control.Monad
import Prelude           hiding (catch)
import Control.Exception
import Data.Char                (toUpper)

import qualified Data.ByteString.Char8 as BS
import qualified System.Exit           as E

type Result  = (Status, BS.ByteString)

type Message = Double -> BS.ByteString

data Health  = Health Double Double deriving (Eq, Show)

data Status
    = OK
      -- ^ The plugin was able to check the service and
      --   it appeared to be functioning properly
    | Warning
      -- ^ The plugin was able to check the service,
      --   but it appeared to be above some "warning"
      --   threshold or did not appear to be working properly
    | Critical
      -- ^ The plugin detected that either the service was
      --   not running or it was above some "critical" threshold
    | Unknown
      -- ^ Invalid command line arguments were supplied
      --   to the plugin or low-level failures internal
      --   to the plugin (such as unable to fork, or open a tcp socket)
      --   that prevent it from performing the specified operation.
      --   Higher-level errors (such as name resolution errors, socket timeouts, etc)
      --   are outside of the control of plugins and should
      --   generally NOT be reported as UNKNOWN states.
      deriving (Eq, Enum, Show)

data Plugin = Plugin
    { service :: BS.ByteString
    , checks  :: [Check]
    }

data Check = Check
    { name     :: BS.ByteString
    , value    :: IO Double
    , health   :: Health
    , ok       :: Message
    , warning  :: Message
    , critical :: Message
    }

--
-- API
--

plugin :: BS.ByteString -> [Check] -> Plugin
plugin = Plugin

check :: Check
check = Check
    { name     = "CHECK"
    , value    = throw $ NoMethodError "Check value not specified"
    , health   = Health 0 0
    , ok       = \_ -> ""
    , warning  = \_ -> ""
    , critical = \_ -> ""
    }

run :: Plugin -> IO ()
run Plugin{..} = do
    (xs, xt) <- mapAndUnzipM exec checks
    let s = precedence xs
    BS.putStrLn $ format service s ""
    BS.putStrLn $ BS.intercalate "\n" xt
    E.exitWith  $ code s

--
-- Private
--

exec :: Check -> IO Result
exec chk@Check{..} = do
    n <- try value
    let (s, t) = status chk n
    return (s, format name s t)

status :: Check -> Either SomeException Double -> Result
status Check{..} (Left e)              = (Unknown, BS.pack $ show e)
status Check{..} (Right n) | n >= y    = (Critical, critical n)
                           | n >= x    = (Warning, warning n)
                           | otherwise = (OK, ok n)
  where
    (Health x y) = health

format :: BS.ByteString -> Status -> BS.ByteString -> BS.ByteString
format name s t = BS.concat [name, " ", BS.pack . map toUpper $ show s, " - ", t]

precedence :: [Status] -> Status
precedence xs | all (== OK) xs     = OK
              | Critical `elem` xs = Critical
              | Unknown `elem` xs  = Unknown
              | Warning `elem` xs  = Warning
              | otherwise          = Unknown

code :: Status -> E.ExitCode
code OK = E.ExitSuccess
code s  = E.ExitFailure (fromEnum s)
