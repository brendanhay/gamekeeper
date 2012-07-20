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

import GameKeeper.Api.Connections
import GameKeeper.Console     (displayInfo)
import GameKeeper.Options
import GameKeeper.Metrics

--
-- API
--

main :: IO ()
main = do
    opts <- parseOptions
    displayInfo "Mode" $ show opts
    runMode opts

--
-- Private
--

runMode :: Options -> IO ()
runMode PushStatistics{..}   = do
    sink <- open optSink
    push sink (Gauge "group" "bucket" (1 :: Int))
    close sink
runMode CleanConnections{..} = do
    resp <- stale optUri optDays
    displayInfo "Response" $ show resp
