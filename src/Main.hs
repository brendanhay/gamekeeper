-- |
-- Module      : Main
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Main (
    -- * Main Entry Point
      main
    ) where

import Control.Concurrent
import Control.Exception.Base (finally)
import System.IO.Unsafe       (unsafePerformIO)
import GameKeeper.API
import GameKeeper.Http
import GameKeeper.Logger
import GameKeeper.Metric
import GameKeeper.Options

--
-- API
--

main :: IO ()
main = do
    opts <- parseOptions
    logDebug $ "Mode: " ++ show opts
    mode opts

--
-- Modes
--

mode :: Options -> IO ()
mode Measure{..} = do
    sink <- open optSink
    forkAll [ showOverview uri >>= push sink
            , listConnections uri >>= idleConnections optDays >>= push sink
            , listChannels uri >>= push sink
            , listExchanges uri >>= mapM_ (push sink)
            , do qs <- listQueues uri
                 push sink $ idleQueues qs
                 mapM_ (push sink) qs
            , listBindings uri >>= push sink
            ]
    wait
    close sink
  where
    uri  = parseUri optUri
mode _ = logError msg >> error msg
  where
    msg = "Unsupported mode"

--
-- Forking
--

children :: MVar [MVar ()]
children = unsafePerformIO (newMVar [])

forkAll :: [IO ()] -> IO ()
forkAll = mapM_ fork

fork :: IO () -> IO ThreadId
fork io = do
    mvar    <- newEmptyMVar
    threads <- takeMVar children
    putMVar children (mvar:threads)
    forkIO (io `finally` putMVar mvar ())

wait :: IO ()
wait = do
    cs <- takeMVar children
    case cs of
        []   -> return ()
        m:ms -> do
            putMVar children ms
            takeMVar m
            wait
