{-# LANGUAGE OverloadedStrings #-}
module Main where

import Auction
import Battle

import Control.Concurrent.MVar (newMVar, readMVar)
import Data.Map (empty)

main :: IO ()
main = do
    auctions <- newMVar empty
    lastAuctionTime <- newMVar 0
    as <- runAuction getNewAuctions (auctions, lastAuctionTime)
    print as
    a <- readMVar auctions
    print a
