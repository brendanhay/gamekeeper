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

import qualified GameKeeper.API.Connection as C
import qualified GameKeeper.API.Queue      as Q

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
runMode PushStatistics{..} = do
    sink <- open optSink
    Q.list (parseUri optUri) >>= emit sink
    close sink
runMode CleanConnections{..} = do
    body <- C.idle (parseUri optUri) optDays
    displayInfo "Response" $ show body
