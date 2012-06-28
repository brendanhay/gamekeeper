{-# LANGUAGE RecordWildCards #-}

module Main
    ( main
    ) where

import GameKeeper.Options

main :: IO ()
main = do
    opts <- parseOptions
    print opts
    exec opts

--
-- Internal
--

exec :: Options -> IO ()
exec PushStatistics{..}   = return ()
exec ShowStatistics{..}   = return ()
exec CleanConnections{..} = return ()