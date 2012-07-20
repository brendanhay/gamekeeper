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
      Options(..)
    , parseOptions
    ) where

import Control.Monad          (when)
import System.Console.CmdArgs
import System.Environment     (getArgs, withArgs)
import System.Exit            (ExitCode(..), exitWith)
import GameKeeper.Metric

data Options =
      PushStatistics
        { optUri :: String
        , optSink :: SinkOptions
        }
    | CleanConnections
        { optUri  :: String
        , optDry  :: Bool
        , optDays :: Integer
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

programName, programVersion, programInfo, copyright :: String
programName    = "gamekeeper"
programVersion = "0.1.0"
programInfo    = programName ++ " version " ++ programVersion
copyright      = "(C) Brendan Hay <brendan@soundcloud.com> 2012"

parse :: Mode (CmdArgs Options)
parse = cmdArgsMode $ modes [pushStatistics, cleanConnections]
    &= versionArg [explicit, name "version", name "v", summary programInfo]
    &= summary (programInfo ++ ", " ++ copyright)
    &= helpArg [explicit, name "help", name "h"]
    &= program programName

validate :: Options -> IO Options
validate opts@PushStatistics{..}   = return opts
validate opts@CleanConnections{..} = return opts
    -- exitWhen (null optUri) "--uri cannot be blank"
    -- return opts

exitWhen :: Bool -> String -> IO ()
exitWhen p msg = when p $ putStrLn msg >> exitWith (ExitFailure 1)

--
-- Modes
--

pushStatistics :: Options
pushStatistics = PushStatistics
    { optUri = defaultUri
        &= name "uri"
        &= typ  "URI"
        &= help "The uri (default: guest@localhost)"
        &= explicit
    , optSink = SinkOptions Stdout "localhost" "5678"
        &= name "sink"
        &= typ  "SINK,HOST,PORT"
        &= help "The sink to write metrics to (default: stdout)"
        &= explicit
    } &= name "push-statistics"
      &= help "Deliver statistics and metrics to the specified sink"

cleanConnections :: Options
cleanConnections = CleanConnections
    { optUri = defaultUri
        &= name "uri"
        &= typ  "URI"
        &= help "The uri (default: guest@localhost)"
        &= explicit
    , optDry = True
        &= name "dry"
        &= help "Dry mode (default: true)"
        &= explicit
    , optDays = 30
        &= name "days"
        &= help "The number of days to prune (default: 30)"
        &= explicit
    } &= name "clean-connections"
      &= help "Perform stale connection cleanup"
      &= explicit

--
-- Defaults
--

defaultUri :: String
defaultUri = "http://guest:guest@127.0.0.1:55672/"
