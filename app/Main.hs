{-# LANGUAGE DoAndIfThenElse #-}
module Main where

import Auction
import Battle
import GMail

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar as MV
import Control.Monad (forever)
import Data.Map as M (empty)
import Data.Time.Clock.POSIX (getPOSIXTime)

main :: IO ()
main = do
    auctionsM <- MV.newMVar M.empty

    lastLoopM <- MV.newMVar 0
    forever $ do
        putStrLn "forking"
        forkIO $ do
            lastLoop <- readMVar lastLoopM
            t <- getPOSIXTime
            if (t - lastLoop >= 15*60) then do
                putStrLn "Checking for new auctions"
                notify <- runAuction auctionsM Battle.update
                if notify then do
                    putStrLn "Notifying"
                    aucs <- readMVar auctionsM
                    sendEmail $ prettyPrintMetadata aucs
                    swapMVar lastLoopM t
                    return ()
                else
                    putStrLn "Don't need to notify"
            else
                putStrLn "Not yet time to check for new auctoins"
        threadDelay $ 5*(10^(6 :: Int))*60
