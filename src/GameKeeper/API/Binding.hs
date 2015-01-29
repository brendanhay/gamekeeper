-- |
-- Module      : GameKeeper.API.Binding
-- Copyright   : (c) 2012-2015 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module GameKeeper.API.Binding (
    -- * Exported Types
      Binding

    -- * HTTP Requests
    , listBindings
    ) where

import Control.Applicative (empty)
import Data.Aeson          (decode')
import Data.Aeson.Types
import Data.Vector         (Vector)
import GameKeeper.Http
import GameKeeper.Metric

data Binding = Binding deriving (Show)

instance FromJSON Binding where
    parseJSON (Array _)  = return Binding
    parseJSON (Object _) = return Binding
    parseJSON _          = empty

instance Measurable [Binding] where
    measure lst = [Gauge group "binding.total" $ len lst]

--
-- API
--

listBindings :: Uri -> IO [Binding]
listBindings uri = list uri "api/bindings" "?columns=" decode
  where
    decode b = decode' b :: Maybe (Vector Binding)
