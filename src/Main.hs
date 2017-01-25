{-# LANGUAGE OverloadedStrings #-}
module Main where

import Battle

main :: IO ()
main = do
    as <- getNewAuctions
    print as
