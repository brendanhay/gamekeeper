-- |
-- Module      : GameKeeper.API.Exchange
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module GameKeeper.API.Exchange (
    Exchange
  , list
  , metrics
  ) where

import Control.Monad.IO.Class
import Control.Applicative ((<$>), (<*>), empty)
import Control.Monad       (liftM, void)
import Data.Aeson          (decode')
import Data.Aeson.Types
import Data.Vector         (Vector, toList)
import GameKeeper.Http
import Network.Metric

import GameKeeper.Metric as M

import qualified Data.ByteString.Char8 as BS

data Exchange = Exchange
    { name      :: BS.ByteString
    , messages  :: Integer
    , consumers :: Integer
    , memory    :: Double
    } deriving (Show)

instance FromJSON Exchange where
    parseJSON (Object o) = Exchange
        <$> o .: "name"
        <*> o .: "messages"
        <*> o .: "consumers"
        <*> liftM megabytes (o .: "memory")
    parseJSON _ = empty

instance Measurable Exchange where
    measure Exchange{..} =
        [ gauge group name "messages" (fromIntegral messages)
        , gauge group name "consumers" (fromIntegral consumers)
        , gauge group name "memory" memory
        ]

--
-- API
--

list :: String -> IO [Exchange]
list uri = do
    body <- getBody $ concat [uri, "api/queues", qs]
    return $ case (decode' body :: Maybe (Vector Exchange)) of
        Just v  -> toList v
        Nothing -> []
  where
    qs = "?columns=name,messages,consumers,memory"

--
-- Private
--

megabytes :: Double -> Double
megabytes = (!! 2) . iterate (/ 1024)
