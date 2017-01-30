module Main
( main
) where

import BattleTest

import Test.HUnit (runTestTT)

main :: IO ()
main = do
    runTestTT testGetSharonsAuctions
    return ()
