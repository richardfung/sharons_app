{-# LANGUAGE OverloadedStrings #-}
module BattleTest
( testGetSharonsAuctions
, testGetUndercuttingAuctions
, testShouldNotify
) where

import Auction
import Battle

import Data.Aeson (decode)
import Data.ByteString.Lazy.Char8 as B (readFile)
import Data.List as L (all, any)
import Data.Map as M ((!), mapWithKey, member)
import Data.Maybe (fromJust, isJust)
import Test.HUnit (Test(TestCase), assertBool)

getTestAuctions :: IO [Auction]
getTestAuctions = do
    auctionsMaybe <- decode <$> B.readFile "auctions.json" :: IO (Maybe Auctions)
    assertBool "auctions.json decode failure" (isJust auctionsMaybe)
    return $ auctions $ fromJust auctionsMaybe

testGetSharonsAuctions :: Test
testGetSharonsAuctions = TestCase $ do
    as <- getTestAuctions
    let sharonsAuctions = getSharonsAuctions [as]

        expectedAuctions = filter ((love ==) . owner) as
        test auction = (M.member (item auction) sharonsAuctions) &&
            (L.any ((auc auction ==) . meta_auc) $ sharonsAuctions M.! (item auction))

    assertBool "Sharons auctions didn't match expected auctions" $
        all test expectedAuctions
    return ()

testGetUndercuttingAuctions :: Test
testGetUndercuttingAuctions = TestCase $ do
    as <- getTestAuctions
    let sharonsAuctions = getSharonsAuctions [as]
        undercuttingAuctions = getUndercuttingAuctions [as] sharonsAuctions
        smallerThanSharons a ss = L.all (\s -> meta_pricePerItem a <= (meta_pricePerItem s)) ss
        test k = L.all (flip smallerThanSharons $ sharonsAuctions M.! k) $ undercuttingAuctions M.! k
    assertBool "Undercutting auctions not smaller than Sharon's auctions" $
        and $ mapWithKey (\k _ -> test k) undercuttingAuctions

testShouldNotify :: Test
testShouldNotify = TestCase $ do
    as <- getTestAuctions
    let sharonsAuctions = getSharonsAuctions [as]
        undercuttingAuctions = getUndercuttingAuctions [as] sharonsAuctions
        currentAuctions = getNewCurrentAuctions sharonsAuctions undercuttingAuctions
    assertBool "Incorrectly notifying on same auctions" $
        shouldNotify currentAuctions currentAuctions
    assertBool "Not notifying when we should" $
        shouldNotify sharonsAuctions currentAuctions
