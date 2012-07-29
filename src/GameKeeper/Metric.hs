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
    , bucket
    , esc
    , len
    , idle

    -- * Re-exports
    , M.SinkType(..)
    , M.Metric(..)
    , M.Measurable(..)
    , M.Encodable(..)
    , M.Sink(..)
    ) where

import Data.Data         (Data, Typeable)
import Data.Word         (Word16)
import Network.Socket
import GameKeeper.Logger

import qualified Data.ByteString.Char8      as BS
import qualified Network.Metric             as M
import qualified Network.Metric.Sink.Handle as H

data SinkOptions = SinkOptions
    { sinkType :: M.SinkType
    , sinkHost :: String
    , sinkPort :: String
    } deriving (Data, Typeable, Show)

-- Temporary until network-metrics is updated
instance Eq M.SinkType where
    M.Stdout == M.Stdout = True
    _ == _               = False

--
-- API
--

open :: SinkOptions -> IO M.AnySink
open SinkOptions{..} = sink
  where
    sink | sinkType == M.Stdout = return . M.AnySink $ H.SinkHandle host logInfo
         | otherwise            = M.open sinkType host sinkHost port
    host = "localhost"
    port = PortNum (read sinkPort :: Word16)

group :: M.Group
group = "rabbit"

bucket :: M.Bucket -> M.Bucket -> M.Bucket
bucket a b = BS.intercalate "." [a, b]

esc :: M.Bucket -> M.Bucket
esc = BS.map fn
  where
    fn '.' = '/'
    fn c   = c

len :: [a] -> Double
len lst = fromIntegral $ length lst :: Double

idle :: [(Bool, a)] -> [a]
idle = map snd . filter ((== True) . fst)