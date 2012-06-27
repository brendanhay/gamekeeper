{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, RecordWildCards #-}

module GameKeeper.Options
    ( Options(..)
    , parseOptions
    ) where

import Control.Monad          (when)
import System.Console.CmdArgs
import System.Environment     (getArgs, withArgs)
import System.Exit            (ExitCode(..), exitWith)

data Options =
    StaleConnections
        { optUri  :: String
        , optDry  :: Bool
        , optDays :: Int
        }
      |
    Monitor
        { optUri :: String
        }
      deriving (Data, Typeable, Show)

parseOptions :: IO Options
parseOptions = do
    raw  <- getArgs
    opts <- (if null raw then withArgs ["--help"] else id) $ cmdArgsRun parse
    validate opts

--
-- Internal
--

_PROGRAM_NAME, _PROGRAM_VERSION, _PROGRAM_INFO, _COPYRIGHT :: String
_PROGRAM_NAME = "gamekeeper"
_PROGRAM_VERSION = "0.1.0"
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_COPYRIGHT = "(C) Brendan Hay <brendan@soundcloud.com> 2012"

parse :: Mode (CmdArgs Options)
parse = cmdArgsMode $ modes [staleConnections, monitor]
    &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
    &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
    &= helpArg [explicit, name "help", name "h"]
    &= program _PROGRAM_NAME

validate :: Options -> IO Options
validate opts@StaleConnections{..} = do
    exitWhen (null optUri) "--uri cannot be blank"
    return opts
validate opts = return opts

exitWhen :: Bool -> String -> IO ()
exitWhen p msg = when p $ putStrLn msg >> exitWith (ExitFailure 1)

staleConnections :: Options
staleConnections = StaleConnections
    { optUri = "http://guest:guest@localhost:55672"
        &= name "uri"
        &= typ  "URI"
        &= help "The uri (default: any)"
        &= explicit
    , optDry = True
        &= name "dry"
        &= help "Dry mode (default: true)"
        &= explicit
    , optDays = 30
        &= name "days"
        &= help "The number of days to prune (default: 30)"
        &= explicit
    }

monitor :: Options
monitor = Monitor
    { optUri = "http://guest:guest@localhost:55672"
        &= name "uri"
        &= typ  "URI"
        &= help "The uri (default: any)"
        &= explicit
    }