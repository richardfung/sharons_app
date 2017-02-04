module Main
( main
) where

import BattleTest

import Test.HUnit (Test(TestList), runTestTT)

main :: IO ()
main = do
    runTestTT tests
    -- runTestTT testPrettyPrintMetadata
    return ()

tests :: Test
tests = TestList [ testGetSharonsAuctions
                 , testGetUndercuttingAuctions
                 , testShouldNotify]
