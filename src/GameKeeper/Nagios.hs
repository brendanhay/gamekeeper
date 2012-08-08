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
    , Service
    , Message
    , Status(..)
    , Check(..)

    -- * Functions
    , check
    ) where

import Data.Char (toUpper)

import qualified Data.ByteString.Char8 as BS
import qualified System.Exit           as E

data Health  = Health Double Double deriving (Eq, Show)

type Service = BS.ByteString

type Message = Double -> BS.ByteString

data Check = Check
    { service  :: Service
    , health   :: Health
    , ok       :: Message
    , warning  :: Message
    , critical :: Message
    , unknown  :: BS.ByteString
    }

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

--
-- API
--

check :: Maybe Double -> Check -> IO ()
check n chk@Check{..} = do
    BS.putStrLn output
    E.exitWith $ code s
  where
    (s, m) = status n chk
    pack   = BS.pack . map toUpper . show
    output = BS.concat [service, " ", pack s, " - ", m]

--
-- Private
--

status :: Maybe Double -> Check -> (Status, BS.ByteString)
status Nothing  Check{..}             = (Unknown, unknown)
status (Just n) Check{..} | n >= x    = (Warning, warning n)
                          | n >= y    = (Critical, critical n)
                          | otherwise = (OK, ok n)
  where
    (Health x y) = health

code :: Status -> E.ExitCode
code OK = E.ExitSuccess
code s  = E.ExitFailure (fromEnum s)
