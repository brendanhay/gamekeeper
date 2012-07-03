{-# LANGUAGE RecordWildCards #-}

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

module Main
    ( main
    ) where

import GameKeeper.Options
import GameKeeper.Connections

--
-- API
--

main :: IO ()
main = do
    opts <- parseOptions
    print opts
    exec opts

--
-- Private
--

exec :: Options -> IO ()
exec PushStatistics{..}   = return ()
exec ShowStatistics{..}   = return ()
exec CleanConnections{..} = connections optUri >>= print
