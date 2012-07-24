-- |
-- Module      : GameKeeper.API.Binding
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module GameKeeper.API.Binding (
    Binding
  , list
  ) where

import Control.Applicative   ((<$>), (<*>), empty)
import Control.Monad         (liftM)
import Data.Aeson            (decode')
import Data.Aeson.Types
import Data.Vector           (Vector)
import Network.Metric
import GameKeeper.Http
import GameKeeper.Metric

data Binding = Binding deriving (Show)

instance FromJSON Binding where
    parseJSON (Object o) = return Binding
    parseJSON _          = empty

instance Measurable [Binding] where
    measure xs = [Gauge group "bindings" len]
      where
        len = fromIntegral $ length xs :: Double

--
-- API
--

list :: Uri -> IO [Binding]
list uri = getList uri { uriPath = path, uriQuery = query } decode
  where
    decode b = decode' b :: Maybe (Vector Binding)
    path     = "api/bindings"
    query    = "?columns="
