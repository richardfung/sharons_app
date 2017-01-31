module Main
( main
) where

import BattleTest

import Test.HUnit (Test(TestList), runTestTT)

main :: IO ()
main = do
    runTestTT testGetSharonsAuctions
    return ()

tests :: Test
tests = TestList [ testGetSharonsAuctions
                 , testGetUndercuttingAuctions]
