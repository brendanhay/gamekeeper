-- |
-- Module      : GameKeeper.API.Node
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module GameKeeper.API.Node (
    -- * Exported Types
      Node(..)

    -- * HTTP Requests
    , showNode
    ) where

import Prelude             hiding (show)
import Control.Applicative ((<$>), (<*>), empty)
import Control.Monad       (liftM)
import Data.Aeson          (decode')
import Data.Aeson.Types
import Data.Maybe          (fromJust)
import GameKeeper.Http

import qualified Data.ByteString.Char8 as BS

data Node = Node
    { node  :: String
    , used  :: Double
    , limit :: Double
    } deriving (Show)

instance FromJSON Node where
    parseJSON (Object o) = Node
        <$> o .: "name"
        <*> liftM gigabytes (o .: "mem_used")
        <*> liftM gigabytes (o .: "mem_limit")
    parseJSON _          = empty

--
-- API
--

showNode :: Uri -> String -> IO Node
showNode uri name = do
    body <- get uri { uriPath = path, uriQuery = query }
    return $ fromJust (decode' body :: Maybe Node)
  where
    path  = BS.concat ["api/nodes/", BS.pack name]
    query = "?columns=name,mem_used,mem_limit"

--
-- Private
--

gigabytes :: Double -> Double
gigabytes = (!! 3) . iterate (/ 1024)
