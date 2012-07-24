{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : GameKeeper.Metric
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module GameKeeper.Metric (
    -- * Exported Types
      SinkOptions(..)

    -- * Sink Constructor
    , open

    -- * Functions
    , group
    , emit

    -- * Re-exports
    , M.SinkType(..)
    , M.Metric(..)
    , M.Measurable
    , M.Encodable
    , M.Sink(..)
    , M.counter
    , M.timer
    , M.gauge
    ) where

import Data.Data       (Data, Typeable)
import Data.Word       (Word16)
import Network.Socket

import qualified Network.Metric as M

data SinkOptions = SinkOptions
    { sinkType :: M.SinkType
    , sinkHost :: String
    , sinkPort :: String
    } deriving (Data, Typeable, Show)
--
-- API
--

open :: SinkOptions -> IO M.AnySink
open SinkOptions{..} = M.open sinkType "localhost" sinkHost port
  where
    port = PortNum (read sinkPort :: Word16)

group :: M.Group
group = "rabbit"

emit :: (M.Sink a, M.Measurable b) => a -> [b] -> IO ()
emit sink = mapM_ (M.push sink)

--
-- Private
--
