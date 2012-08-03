{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : GameKeeper.ExplicitOptions
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Main where

import Data.Version                    (showVersion)
import Paths_gamekeeper                (version)
import System.Console.CmdArgs.Explicit
import System.Environment              (getArgs)
import GameKeeper.Http
import GameKeeper.Metric        hiding (measure)

data Health = Health
    { warnLevel :: Double
    , critLevel :: Double
    } deriving (Eq, Show)

data Options
    = Help (Mode Options)
    | Version
    | Measure
      { optUri      :: Uri
      , optDays     :: Int
      , optSink     :: SinkOptions
      }
    | PruneConnections
      { optUri      :: Uri
      , optDays     :: Int
      }
    | PruneQueues
      { optUri      :: Uri
      }
    | CheckNode
      { optUri      :: Uri
      , optMessages :: Health
      , optMemory   :: Health
      }
    | CheckQueue
      { optUri      :: Uri
      , optMessages :: Health
      , optMemory   :: Health
      }
    deriving (Show)

--
-- Info
--

programName, programInfo :: String
programName = "gamekeeper"
programInfo = concat
    [ programName
    , " version "
    , showVersion version
    , " (C) Brendan Hay <brendan@soundcloud.com> 2012"
    ]

main :: IO ()
main = do
    args <- getArgs
    case processValue program args of
        (Help m) -> print $ helpText [] HelpFormatOne m
        Version  -> putStrLn programInfo
        opts -> print opts

--
-- Modes
--

measure :: Mode Options
measure = child
    "measure"
    (Measure defUri defDays (SinkOptions Stdout "" ""))
    "Measure stuff"
    [uriFlag]

pruneConnections :: Mode Options
pruneConnections = child
    "connections"
    (PruneConnections defUri defDays)
    "Prune connections"
    [uriFlag]

pruneQueues :: Mode Options
pruneQueues = child
    "queues"
    (PruneQueues defUri)
    "Prune queues"
    [uriFlag]

prune :: Mode Options
prune = parent
    "prune"
    (Help prune)
     "Prune mode"
    []
    [pruneConnections, pruneQueues]

checkNode :: Mode Options
checkNode = child
    "node"
    (CheckNode defUri defHealth defHealth)
    "Check node"
    [uriFlag]

checkQueue :: Mode Options
checkQueue = child
    "queue"
    (CheckQueue defUri defHealth defHealth)
    "Check queue"
    [uriFlag]

check :: Mode Options
check = parent
    "check"
    (Help check)
    "Check mode"
    []
    [checkNode, checkQueue]

program :: Mode Options
program = parent
    programName
    (Help program)
    "Program help"
    [flagVersion (\_ -> Version)]
    [measure, prune, check]

--
-- Constructors
--

parent :: Name -> Options -> Help -> [Flag Options] -> [Mode Options] -> Mode Options
parent name value help flags groups = mode'
  where
    mode' = (modeEmpty value)
        { modeNames      = [name]
        , modeHelp       = help
        , modeArgs       = ([], Nothing)
        , modeGroupFlags = toGroup $ defFlags mode' flags
        , modeGroupModes = toGroup groups
        }

child :: Name -> Options -> Help -> [Flag Options] -> Mode Options
child name value help flags = mode'
  where
    mode' = mode name value help err $ defFlags mode' flags
    err = flagArg (\x _ -> Left $ "Unexpected argument " ++ x) ""

--
-- Flags
--

uriFlag :: Flag Options
uriFlag = flagReq ["uri"] (\x b -> Right $ b { optUri = parseUri x }) "URI" "URI of the RabbitMQ HTTP API (default: guest@localhost:55672)"

helpFlag :: a -> Flag a
helpFlag m = flagNone ["help", "h"] (\_ -> m) "Display this help message"

--
-- Defaults
--

defUri :: Uri
defUri = parseUri "http://guest:guest@127.0.0.1:55672/"

defHealth :: Health
defHealth = Health 1 10

defFlags :: Mode Options -> [Flag Options] -> [Flag Options]
defFlags m = (++ [helpFlag $ Help m])

defDays :: Int
defDays = 30