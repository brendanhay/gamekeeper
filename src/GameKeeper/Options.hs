-- |
-- Module      : GameKeeper.Options
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module GameKeeper.Options (
    -- * Exported Types
      Options(..)
    , Resource(..)

    -- * Functions
    , parseOptions
    ) where

import Control.Monad            (when)
import Data.Version             (showVersion)
import Paths_gamekeeper         (version)
import System.Console.CmdArgs
import System.Environment       (getArgs, withArgs)
import System.Exit              (ExitCode(..), exitWith)
import GameKeeper.Metric hiding (measure)

data Resource
    = ConnectionResource
    | QueueResource
      deriving (Data, Typeable, Eq, Show)

data Options
    = Measure
      { optUri  :: String
      , optDays :: Int
      , optSink :: SinkOptions
      }
    | Clean
      { optUri      :: String
      , optResource :: Resource
      , optDays     :: Int
      , optDry      :: Bool
      }
    deriving (Data, Typeable, Show)

--
-- API
--

parseOptions :: IO Options
parseOptions = do
    raw  <- getArgs
    opts <- (if null raw then withArgs ["--help"] else id) $ cmdArgsRun parse
    validate opts

--
-- Parsing
--

programName, programInfo, copyright :: String
programName = "gamekeeper"
programInfo = programName ++ " version " ++ showVersion version
copyright   = "(C) Brendan Hay <brendan@soundcloud.com> 2012"

parse :: Mode (CmdArgs Options)
parse = cmdArgsMode $ modes [measure, clean]
    &= versionArg [explicit, name "version", name "v", summary programInfo]
    &= summary (programInfo ++ ", " ++ copyright)
    &= helpArg [explicit, name "help", name "h"]
    &= program programName

validate :: Options -> IO Options
validate opts@Measure{..} = return opts
validate opts@Clean{..} = return opts

    -- exitWhen (null optUri) "--uri cannot be blank"
    -- return opts

exitWhen :: Bool -> String -> IO ()
exitWhen p msg = when p $ putStrLn msg >> exitWith (ExitFailure 1)

--
-- Modes
--

measure :: Options
measure = Measure
    { optUri = defaultUri
        &= name "uri"
        &= typ  "URI"
        &= help "The uri (default: guest@localhost)"
        &= explicit
    , optDays = 1
        &= name "days"
        &= help "The number of days inactivity after which a resource is considered idle (default: 1)"
        &= explicit
    , optSink = SinkOptions Stdout "" ""
        &= name "sink"
        &= typ  "SINK,HOST,PORT"
        &= help "The sink (SINK: ganglia|graphite|statsd|stdout) to write metrics to (default: stdout)"
        &= explicit
    } &= name "measure"
      &= help "Deliver statistics and metrics to the specified sink"

clean :: Options
clean = Clean
    { optUri = defaultUri
        &= name "uri"
        &= typ  "URI"
        &= help "The uri (default: guest@localhost)"
        &= explicit
    , optResource = ConnectionResource
        &= name "resource"
        &= typ "RESOURCE"
        &= help "The resource (RESOURCE: connection|queue) to cleanup (default: connection)"
        &= explicit
    , optDry = True
        &= name "dry"
        &= help "Dry mode (default: true)"
        &= explicit
    , optDays = 1
        &= name "days"
        &= help "The number of days inactivity after which a resource is considered idle (default: 1)"
        &= explicit
    } &= name "clean"
      &= help "Perform idle resource pruning"
      &= explicit

--
-- Defaults
--

defaultUri :: String
defaultUri = "http://guest:guest@127.0.0.1:55672/"
