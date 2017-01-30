{-# LANGUAGE OverloadedStrings #-}
module BattleTest
( testGetSharonsAuctions
) where

import Auction
import Battle

import Data.Aeson (decode)
import Data.ByteString.Lazy.Char8 as B (readFile)
import Data.List as L (any)
import Data.Map as M ((!), member)
import Data.Maybe (fromJust, isJust)
import Test.HUnit (Test(TestCase), assertBool)

testGetSharonsAuctions :: Test
testGetSharonsAuctions = TestCase $ do
    auctionsMaybe <- decode <$> B.readFile "auctions.json" :: IO (Maybe Auctions)
    assertBool "auctions.json decode failure" (isJust auctionsMaybe)
    let as = auctions $ fromJust auctionsMaybe
        sharonsAuctions = getSharonsAuctions [as]

        expectedAuctions = filter ((love ==) . owner) as
        test auction = (M.member (item auction) sharonsAuctions) &&
            (L.any ((auc auction ==) . meta_auc) $ sharonsAuctions M.! (item auction))

    assertBool "Sharons auctions didn't match expected auctions" $
        all test expectedAuctions
    return ()
