{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : GameKeeper.Options
-- Copyright   : (c) 2012-2015 Brendan Hay <brendan@soundcloud.com>
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

import Data.List.Split                        (splitOn)
import Data.Word                              (Word16)
import Data.Version                           (showVersion)
import Paths_gamekeeper                       (version)
import System.Console.CmdArgs.Explicit hiding (modes)
import System.Console.CmdArgs.Verbosity
import System.Environment                     (getArgs)
import System.IO.Unsafe                       (unsafePerformIO)
import GameKeeper.Http
import GameKeeper.Metric               hiding (measure)
import GameKeeper.Nagios                      (Health(..))

import qualified Data.ByteString.Char8 as BS

data Options
    = Help SubMode
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
      , optName     :: BS.ByteString
      , optMessages :: Health
      , optMemory   :: Health
      }
    | CheckQueue
      { optUri      :: Uri
      , optName     :: BS.ByteString
      , optMessages :: Health
      , optMemory   :: Health
      }
    deriving (Show)

data SubMode = SubMode
    { name  :: String
    , def   :: Options
    , help  :: String
    , flags :: [Flag Options]
    , modes :: [SubMode]
    } deriving (Show)

--
-- API
--

parseOptions :: IO (Either String Options)
parseOptions = do
    args <- getArgs
    return $ case processValue (expandMode program) args of
        (Help m) -> Left . show $ helpText [] HelpFormatOne (expandMode m)
        Version  -> Left programInfo
        opts     -> validate opts

--
-- Private
--

programName, programInfo :: String
programName = "gamekeeper"
programInfo = concat
    [ programName
    , " version "
    , showVersion version
    , " (C) Brendan Hay <brendan@soundcloud.com> 2012"
    ]

validate :: Options -> Either String Options
validate opts@CheckNode{..}  = when opts [(BS.null optName, "--name cannot be blank")]
validate opts@CheckQueue{..} = when opts [(BS.null optName, "--name cannot be blank")]
validate opts                = Right opts

when :: Options -> [(Bool, String)] -> Either String Options
when opts = f . filter ((== True) . fst)
  where
    f []         = Right opts
    f ((_, s):_) = Left s

uri :: Uri
uri = parseUri "http://guest:guest@127.0.0.1:55672/"

health, messages, memory :: Double -> Health
health crit = Health (fromInteger . floor $ crit / 2) crit
messages    = health
memory      = health

oneMonth :: Int
oneMonth = 30

quarterMillion, thirtyMillion :: Double
quarterMillion = 250000
thirtyMillion  = 30000000

subMode :: SubMode
subMode = SubMode "" (Help program) "" [] []

expandMode :: SubMode -> Mode Options
expandMode m@SubMode{..} | null modes = child
                         | otherwise  = parent
  where
    errFlag = flagArg (\x _ -> Left $ "Unexpected argument " ++ x) ""
    child   = mode name def help errFlag $ appendDefaults m flags
    parent  = (modeEmpty def)
        { modeNames      = [name]
        , modeHelp       = help
        , modeArgs       = ([], Nothing)
        , modeGroupFlags = toGroup $ appendDefaults m flags
        , modeGroupModes = toGroup $ map expandMode modes
        }

program :: SubMode
program = subMode
    { name  = programName
    , def   = Help program
    , help  = "Program help"
    , flags = [flagVersion (const Version)]
    , modes = [measure, prune, check]
    }

--
-- Modes
--

measure :: SubMode
measure = subMode
    { name  = "measure"
    , def   = Measure uri oneMonth defaultSinkOpts
    , help  = "Measure and emit metrics to the specified sink"
    , flags = [ uriFlag
              , daysFlag "Number of days before a connection is considered idle"
              , sinkFlag "Sink options describing the type and host/port combination"
              ]
    }

pruneConnections :: SubMode
pruneConnections = subMode
    { name  = "connections"
    , def   = PruneConnections uri oneMonth
    , help  = "Perform idle connection pruning"
    , flags = [ uriFlag
              , daysFlag "Number of days before a connection is considered idle"
              ]
    }

pruneQueues :: SubMode
pruneQueues = subMode
    { name  = "queues"
    , def   = PruneQueues uri
    , help  = "Perform inactive queue pruning"
    , flags = [uriFlag]
    }

prune :: SubMode
prune = subMode
    { name  = "prune"
    , def   = Help prune
    , help  = "Prune mode"
    , modes = [pruneConnections, pruneQueues]
    }

checkNode :: SubMode
checkNode = subMode
    { name  = "node"
    , def   = CheckNode uri "" (messages thirtyMillion) (memory 8)
    , help  = "Check a node's memory and message residence"
    , flags = [ uriFlag
              , nameFlag "ATOM" "Erlang node name"
              , messagesFlag "Message residence thresholds"
              , memoryFlag "Memory usage thresholds (measurement: GB)"
              ]
    }

checkQueue :: SubMode
checkQueue = subMode
    { name  = "queue"
    , def   = CheckQueue uri "" (messages quarterMillion) (memory 500)
    , help  = "Check a queue's memory and message residence"
    , flags = [ uriFlag
              , nameFlag "STR" "AMQP Queue name"
              , messagesFlag "Message residence thresholds"
              , memoryFlag "Memory usage thresholds (measurement: MB)"
              ]
    }

check :: SubMode
check = subMode
    { name  = "check"
    , def   = Help check
    , help  = "Check stuff"
    , modes = [checkNode, checkQueue]
    }

--
-- Flags
--

appendDefaults :: SubMode -> [Flag Options] -> [Flag Options]
appendDefaults m = (++ (helpFlag (Help m) : verbosityFlags))

verbosityFlags :: [Flag Options]
verbosityFlags = flagsVerbosity f
  where
    f v o = unsafePerformIO (setVerbosity v >> return o)
{-# NOINLINE verbosityFlags #-}

uriFlag :: Flag Options
uriFlag = flagReq ["uri"] (\s o -> Right o { optUri = parseUri s }) "URI" help
  where
    help = "URI of the RabbitMQ HTTP API (default: guest@localhost:15672)"

nameFlag :: String -> String -> Flag Options
nameFlag = flagReq ["name"] (\s o -> Right o { optName = BS.pack s })

helpFlag :: a -> Flag a
helpFlag m = flagNone ["help", "h"] (const m) "Display this help message"

daysFlag :: String -> Flag Options
daysFlag = flagReq ["days"] (\s o -> Right $ o { optDays = read s :: Int }) "INT"

healthFlag :: String
           -> (Options -> Double -> Double -> Options)
           -> String
           -> Flag Options
healthFlag name upd = flagReq [name] f "WARN,CRIT"
  where
    f s o = Right $ upd o warn crit
      where
        [warn, crit] = map read $ splitOn "," s :: [Double]

messagesFlag :: String -> Flag Options
messagesFlag = healthFlag "messages" upd
  where
    upd o w c = o { optMessages = Health w c }

memoryFlag :: String -> Flag Options
memoryFlag = healthFlag "memory" upd
  where
    upd o w c = o { optMemory = Health w c }

sinkFlag :: String -> Flag Options
sinkFlag = flagReq ["sink"] upd "SINK,HOST,PORT"
  where
    upd s o = Right o
        { optSink = case splitOn "," s of
              (x:y:z:_) -> SinkOptions (read x :: SinkType) y (read z :: Word16)
              _         -> error $ "Failed to parse SinkOptions from SINK,HOST,PORT from: " ++ s
        }
