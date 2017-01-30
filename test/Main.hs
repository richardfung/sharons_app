module Main
( main
) where

import BattleTest

import Control.Concurrent.MVar (newMVar, readMVar)
import Data.Map (empty)
import Test.HUnit (runTestTT)

main :: IO ()
main = do
    auctions <- newMVar empty
    lastAuctionTime <- newMVar 0
    counts <- runTestTT testGetAuctionFiles
    return ()
