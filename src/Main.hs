{-# LANGUAGE OverloadedStrings #-}
module Main where

import Battle

import Control.Applicative
import Snap.Core
-- import Snap.Util.FileServe
-- import Snap.Http.Server

main :: IO ()
main = do
    af <- getAuctionFiles
    print af
    --quickHttpServe site

site :: Snap ()
site =
    ifTop (writeBS "hello world") <|>
    route [ ("foo", writeBS "bar")
          ]
