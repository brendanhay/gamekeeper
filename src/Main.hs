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
import GameKeeper.Console     (displayInfo)
import GameKeeper.Http        (parseUri)
import GameKeeper.Metric
import GameKeeper.Options

import qualified GameKeeper.API.Overview   as O
import qualified GameKeeper.API.Connection as C
import qualified GameKeeper.API.Channel    as CH
import qualified GameKeeper.API.Exchange   as E
import qualified GameKeeper.API.Binding    as B
import qualified GameKeeper.API.Queue      as Q

--
-- API
--

main :: IO ()
main = do
    opts <- parseOptions
    displayInfo "Mode" $ show opts
    mode opts

--
-- Private
--

mode :: Options -> IO ()
mode Measure{..} = do
    sink <- open optSink
    mapM_ fork [ C.list uri >>= C.idle optDays >>= push sink
               , CH.list uri >>= push sink
               , E.list uri >>= mapM_ (push sink)
               , B.list uri >>= push sink
               , Q.list uri >>= mapM_ (push sink)
               , O.show uri >>= push sink
               ]
    wait
    close sink
  where
    uri = parseUri optUri
mode _ = error "Unsupported mode"

children :: MVar [MVar ()]
children = unsafePerformIO (newMVar [])

wait :: IO ()
wait = do
    cs <- takeMVar children
    case cs of
        []   -> return ()
        m:ms -> do
            putMVar children ms
            takeMVar m
            wait

fork :: IO () -> IO ThreadId
fork io = do
    mvar    <- newEmptyMVar
    threads <- takeMVar children
    putMVar children (mvar:threads)
    forkIO (io `finally` putMVar mvar ())