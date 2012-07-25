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

import GameKeeper.Console (displayInfo)
import GameKeeper.Http    (parseUri)
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

    putStrLn "Connection Metrics:"
    C.list uri >>= C.idle optDays >>= push sink

    putStrLn "Channel Metrics:"
    CH.list uri >>= push sink

    putStrLn "Exchange Metrics:"
    E.list uri >>= mapM_ (push sink)

    putStrLn "Binding Metrics:"
    B.list uri >>= push sink

    putStrLn "Queue Metrics:"
    Q.list uri >>= mapM_ (push sink)

    putStrLn "Overview Metrics:"
    O.show uri >>= push sink

    close sink
  where
    uri = parseUri optUri
mode _ = error "Unsupported mode"
