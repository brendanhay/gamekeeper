-- |
-- Module      : GameKeeper.Logger
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module GameKeeper.Logger (
    -- * Severity Functions
      logDebug
    , logInfo
    , logError
    ) where

import Prelude hiding (log)
import Data.Time
import System.Locale

data Severity = Debug | Info | Error deriving (Show)

--
-- API
--

logDebug, logInfo, logError :: String -> IO ()
logDebug = log Debug
logInfo  = log Info
logError = log Error

--
-- Private
--

log :: Severity -> String -> IO ()
log m s = getCurrentTime >>= putStrLn . unwords . format
  where
    locale   = defaultTimeLocale
    format t = [short m, formatTime locale "[%FT%T.%q]" t, long m, "-- :", s]

short :: Severity -> String
short Debug = "D,"
short Info  = "I,"
short Error = "E,"

long :: Severity -> String
long Debug = "DEBUG"
long Info  = " INFO"
long Error = "ERROR"
