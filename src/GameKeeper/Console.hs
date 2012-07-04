{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : GameKeeper.Console
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module GameKeeper.Console (
    displayInfo
  ) where

--
-- API
--

displayInfo :: String -> String -> IO ()
displayInfo action output = putStrLn $ concat [pad (action ++ ": "), output]

--
-- Private
--

pad :: String -> String
pad line =
    let linelen = length line in
        if linelen <= width
            then line ++ replicate (width - linelen) ' '
            else take width line
  where
    width = 12