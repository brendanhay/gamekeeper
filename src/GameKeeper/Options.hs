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
    , Health(..)

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

data Health = Health
    { warning  :: Double
    , critical :: Double
    } deriving (Data, Typeable, Eq, Show)

data Options
    = Measure
      { optUri  :: String
      , optDays :: Int
      , optSink :: SinkOptions
      }
    | PruneConnections
      { optUri      :: String
      , optDays     :: Int
      }
    | PruneQueues
      { optUri      :: String
      }
    | CheckQueue
      { optUri      :: String
      , optMessages :: Health
      , optMemory   :: Health
      }
    | CheckNode
      { optUri      :: String
      , optMessages :: Health
      , optMemory   :: Health
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
parse = cmdArgsMode $ modes [ measure
                            , pruneConnections
                            , pruneQueues
                            , checkNode
                            , checkQueue
                            ]
    &= versionArg [explicit, name "version", name "v", summary programInfo]
    &= summary (programInfo ++ ", " ++ copyright)
    &= helpArg [explicit, name "help", name "h"]
    &= program programName

validate :: Options -> IO Options
validate opts = return opts
-- validate opts@PruneConnections{..} = return opts
-- validate opts@PruneQueues{..}      = return opts
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
      &= help "Deliver metrics to the specified sink"

pruneConnections :: Options
pruneConnections = PruneConnections
    { optUri = defaultUri
        &= name "uri"
        &= typ  "URI"
        &= help "The uri (default: guest@localhost)"
        &= explicit
    , optDays = 1
        &= name "days"
        &= help "The number of days inactivity after which a connection is considered idle (default: 1)"
        &= explicit
    } &= name "prune-connections"
      &= help "Perform idle connection pruning"
      &= explicit

pruneQueues :: Options
pruneQueues = PruneQueues
    { optUri = defaultUri
        &= name "uri"
        &= typ  "URI"
        &= help "The uri (default: guest@localhost)"
        &= explicit
    } &= name "prune-queues"
      &= help "Perform inactive queue pruning"
      &= explicit

checkNode :: Options
checkNode = CheckNode
    { optUri = defaultUri
        &= name "uri"
        &= typ  "URI"
        &= help "The uri (default: guest@localhost)"
        &= explicit
    , optMessages = Health 1 2
        &= name "messages"
        &= typ  "WARN,CRIT"
        &= help "The total number of messages (measurement: double)"
        &= explicit
    , optMemory = Health 1024 2028
        &= name "memory"
        &= typ  "WARN,CRIT"
        &= help "The total amount of memory in use as reported by the VM (measurement: megabytes)"
        &= explicit
    } &= name "check-node"
      &= help "Check general node health"

checkQueue :: Options
checkQueue = CheckQueue
    { optUri = defaultUri
        &= name "uri"
        &= typ  "URI"
        &= help "The uri (default: guest@localhost)"
        &= explicit
    , optMessages = Health 50000 100000
        &= name "messages"
        &= typ  "WARN,CRIT"
        &= help "The total number of messages for a specific queue (measurement: double)"
        &= explicit
    , optMemory = Health 1024 2028
        &= name "memory"
        &= typ  "WARN,CRIT"
        &= help "The amount of memory in use by a specific queue (measurement: megabytes)"
        &= explicit
    } &= name "check-queue"
      &= help "Check a specific queue's health"

--
-- Defaults
--

defaultUri :: String
defaultUri = "http://guest:guest@127.0.0.1:55672/"
