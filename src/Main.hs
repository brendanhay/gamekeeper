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
      main
    ) where

import Control.Concurrent
import Control.Exception.Base (finally)
import System.IO.Unsafe
import GameKeeper.Http        (parseUri)
import GameKeeper.Logger
import GameKeeper.Metric
import GameKeeper.Options

import qualified GameKeeper.API.Overview   as Over
import qualified GameKeeper.API.Connection as Conn
import qualified GameKeeper.API.Channel    as Chan
import qualified GameKeeper.API.Exchange   as Exch
import qualified GameKeeper.API.Binding    as Bind
import qualified GameKeeper.API.Queue      as Queu

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
    forkAll [ Over.show uri >>= push sink
            , Conn.list uri >>= Conn.idle optDays >>= push sink
            , Chan.list uri >>= push sink
            , Exch.list uri >>= mapM_ (push sink)
            , Queu.list uri >>= mapM_ (push sink)
            , Bind.list uri >>= push sink
            ]
    wait
    close sink
  where
    uri = parseUri optUri
mode _ = logError msg >> error msg
  where
    msg = "Unsupported mode"

--
-- Forking
--

children :: MVar [MVar ()]
children = unsafePerformIO (newMVar [])

forkAll :: [IO ()] -> IO ()
forkAll = mapM_ f
  where
    f g = fork g >>= logDebug . ("Spark: " ++) . show

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
