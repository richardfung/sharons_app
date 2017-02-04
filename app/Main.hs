module Main where

import Auction
import Battle
import GMail

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar as MV
import Control.Monad (forever, when)
import Data.Map as M (empty)
import Data.Time.Clock.POSIX (getPOSIXTime)

main :: IO ()
main = do
    auctionsM <- MV.newMVar M.empty
    auctionTimeM <- MV.newMVar 0 :: IO (MV.MVar Int)

    lastLoopM <- MV.newMVar 0
    forever $ do
        forkIO $ do
            lastLoop <- readMVar lastLoopM
            t <- getPOSIXTime
            when (t - lastLoop >= 15*60) $ do
                notify <- runAuction (auctionsM, auctionTimeM) Battle.update
                when notify $ do
                    aucs <- readMVar auctionsM
                    sendEmail $ prettyPrintMetadata aucs
                    swapMVar lastLoopM t
                    return ()
        threadDelay $ 5*(10^(6 :: Int))*60
