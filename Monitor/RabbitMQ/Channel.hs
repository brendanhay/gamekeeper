{-# LANGUAGE DeriveDataTypeable #-}

module Monitor.RabbitMQ.Channel (
    Channels(..)
  ) where

import Control.Monad             (mzero)
import Data.Data                 (Data, Typeable)
import Data.Aeson                (FromJSON(..))
import Data.Aeson.Types
import Monitor.RabbitMQ.Retrieve (Retrieve(..))

import qualified Data.Vector as V

data Channels = Channels Int deriving (Show, Data, Typeable)

instance FromJSON Channels where
    parseJSON (Array o) = return $ Channels $ V.length o
    parseJSON _ = mzero

instance Retrieve Channels where
    columns _ = []
