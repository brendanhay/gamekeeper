{-# LANGUAGE RecordWildCards #-}

module Main
    ( main
    ) where

import GameKeeper.Options
import GameKeeper.Connections

--
-- API
--

main :: IO ()
main = do
    opts <- parseOptions
    print opts
    exec opts

--
-- Private
--

exec :: Options -> IO ()
exec PushStatistics{..}   = return ()
exec ShowStatistics{..}   = return ()
exec CleanConnections{..} = connections optUri >>= print
