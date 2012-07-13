{-# LANGUAGE ExistentialQuantification #-}

-- |
-- Module      : GameKeeper.Sink
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module GameKeeper.Sink (
      Metric(..)
    , SinkType(..)
    , Sink(..)
    , open
    ) where

import Data.Data (Data, Typeable)

-- import qualified Data.ByteString.Char8 as BS

data Metric = Metric String String
    deriving (Show)

data SinkType = Ganglia | Graphite | Stdout
    deriving (Data, Typeable, Show)

data Sink = Sink
    { push :: Metric -> IO ()
    }

--
-- API
--

open :: SinkType -> IO Sink
open Ganglia  = return $ Sink (\(Metric k v) -> print [k, v])
open Graphite = return $ Sink (\(Metric k v) -> print [k, v])
open Stdout   = return $ Sink (\(Metric k v) -> print [k, v])

--
-- Private
--
