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
import Control.Monad          (liftM)
import System.IO.Unsafe       (unsafePerformIO)
import GameKeeper.API
import GameKeeper.Logger
import GameKeeper.Metric
import GameKeeper.Nagios
import GameKeeper.ExplicitOptions

--
-- API
--

main :: IO ()
main = parseOptions >>= either putStrLn run
  where
    run o = do logDebug $ "Mode: " ++ show o; mode o

--
-- Modes
--

mode :: Options -> IO ()
mode Measure{..} = do
    sink <- open optSink
    forkAll [ showOverview optUri >>= push sink
            , listConnections optUri >>= idleConnections optDays >>= push sink
            , listChannels optUri >>= push sink
            , listExchanges optUri >>= mapM_ (push sink)
            , do qs <- listQueues optUri
                 push sink $ idleQueues qs
                 mapM_ (push sink) qs
            , listBindings optUri >>= push sink
            ]
    wait
    close sink

mode PruneConnections{..} = do
    cs <- liftM idle (listConnections optUri >>= idleConnections optDays)
    putStrLn $ "Idle Connections Found: " ++ show (length cs)
    mapM_ (deleteConnection optUri) cs

mode PruneQueues{..} = do
    qs <- listQueues optUri
    mapM_ (deleteQueue optUri) . idle $ unusedQueues qs

mode CheckNode{..} = do
    (Overview cnt _) <- showOverview optUri
    f cnt optMessages
  where
    f n Health{..} | total n >= healthWarn = warning
                   | total n >= healthCrit = critical
                   | otherwise             = ok

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
