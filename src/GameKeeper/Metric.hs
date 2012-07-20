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

    -- * Sink Functions
    , M.Sink(push, mpush, close)
    , open

    -- * Re-exports
    , M.SinkType(..)
    , M.Metric(..)
    , M.Encodable
    ) where

import Data.Data (Data, Typeable)

import qualified Network.Metric as M

data SinkOptions = SinkOptions
    { sinkType :: M.SinkType
    , sinkHost :: String
    , sinkPort :: String
    } deriving (Data, Typeable, Show)

--
-- API
--

open :: SinkOptions -> IO M.MetricSink
open SinkOptions{..} = M.open sinkType sinkHost sinkPort

--
-- Private
--
