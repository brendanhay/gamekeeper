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
    , SinkOptions(..)
    , Sink(push, close)
    , mkSink
    ) where

import Data.Data (Data, Typeable)

import qualified Data.ByteString.Char8    as BS
import qualified Network.Metrics.Ganglia  as Ganglia
import qualified Network.Metrics.Graphite as Graphite
import qualified Network.Metrics.Statsd   as Statsd

data Metric = Metric BS.ByteString BS.ByteString deriving (Show)

data SinkType = Ganglia | Graphite | Statsd | Stdout deriving (Data, Typeable, Show)

type Writer = Metric -> IO ()

data SinkOptions = SinkOptions
    { sinkType :: SinkType
    , sinkHost :: String
    , sinkPort :: String
    } deriving (Data, Typeable, Show)

data Sink = Sink
    { push  :: Writer
    , close :: IO ()
    }

--
-- API
--

mkSink :: SinkOptions -> IO Sink
mkSink opts@SinkOptions{..} = fn opts
  where
    fn = case sinkType of
        Ganglia  -> gangliaSink
        Graphite -> graphiteSink
        Statsd   -> stdoutSink
        Stdout   -> stdoutSink

-- open Graphite = return $ Sink (\(Metric k v) -> print [k, v]) (return ())
-- open Statsd   = return $ Sink (\(Metric k v) -> print [k, v]) (return ())
-- open Stdout   = return $ Sink (\(Metric k v) -> print [k, v]) (return ())

--
-- Private
--

gangliaSink :: SinkOptions -> IO Sink
gangliaSink SinkOptions{..} = do
    handle <- Ganglia.open sinkHost sinkPort
    return $ Sink (push handle) (Ganglia.close handle)
  where
    conv (Metric k v) = Ganglia.defaultMetric { Ganglia.name = k, Ganglia.value = v }
    push hd m         = Ganglia.emit (conv m) hd >> return ()

graphiteSink :: SinkOptions -> IO Sink
graphiteSink SinkOptions{..} = do
    handle <- Graphite.open sinkHost sinkPort
    return $ Sink (push handle) (Graphite.close handle)
  where
    conv (Metric k v) = Graphite.Metric k v
    push hd m         = Graphite.emit (conv m) hd >> return ()

stdoutSink :: SinkOptions -> IO Sink
stdoutSink _ = return $ Sink (\(Metric k v) -> print [k, v]) (return ())