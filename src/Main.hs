module Main
    ( main
    ) where

import GameKeeper.Options

main :: IO ()
main = do
    opts <- parseOptions
    print opts

--     lookup command dispatch

-- dispatch :: [(String, [String] -> IO ())]
-- dispatch =  [ ("counts", counts)
--             , ("overview", overview)
--             ]