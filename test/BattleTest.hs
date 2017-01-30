module BattleTest
( testGetAuctionFiles
) where

import Battle

import Test.HUnit

testGetAuctions = TestCase $ do
    getAuctionFiles
    return ()
