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
    , Service(..)
    , Status(..)
    , Check(..)

    -- * Functions
    , exitWith
    ) where

import Data.Char (toUpper)
import Data.Data (Data, Typeable)
import Data.Word (Word16)
import Network.Socket
import GameKeeper.Logger

import qualified Data.ByteString.Char8 as BS
import qualified System.Exit           as E

data Health = Health
    { healthWarn :: Double
    , healthCrit :: Double
    } deriving (Eq, Show)

newtype Service = Service BS.ByteString deriving (Show)

data Status
    = OK       -- ^ The plugin was able to check the service and
               --   it appeared to be functioning properly
    | Warning  -- ^ The plugin was able to check the service,
               --   but it appeared to be above some "warning"
               --   threshold or did not appear to be working properly
    | Critical -- ^ The plugin detected that either the service was
               --   not running or it was above some "critical" threshold
    | Unknown  -- ^ Invalid command line arguments were supplied
               --   to the plugin or low-level failures internal
               --   to the plugin (such as unable to fork, or open a tcp socket)
               --   that prevent it from performing the specified operation.
               --   Higher-level errors (such as name resolution errors, socket timeouts, etc)
               --   are outside of the control of plugins and should
               --   generally NOT be reported as UNKNOWN states.
      deriving (Eq, Enum, Show)

data Check = Check
    { service :: Service
    , status  :: Status
    , text    :: BS.ByteString
    } deriving (Show)

--
-- API
--

exitWith :: Check -> IO ()
exitWith chk = BS.putStrLn (toByteString chk) >> E.exitWith (toExitCode chk)

--
-- Private
--

toExitCode :: Check -> E.ExitCode
toExitCode Check{..} | status == OK = E.ExitSuccess
                     | otherwise    = E.ExitFailure (fromEnum status)

toByteString :: Check -> BS.ByteString
toByteString Check{..} = BS.concat [bserv service, " ", bstat status, " - ", text]
  where
    bserv (Service s) = s
    bstat             = BS.pack . map toUpper . show

