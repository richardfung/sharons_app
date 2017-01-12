{-# LANGUAGE OverloadedStrings #-}
module Main where

import Battle

import Control.Applicative
import qualified Data.ByteString.Char8 as B
import Snap.Core
-- import Snap.Util.FileServe
-- import Snap.Http.Server

main :: IO ()
main = do
    af <- getAuctionFiles
    print af
    a <- getAuction $ B.pack $ url $ head $ files af
    print a
    --quickHttpServe site

site :: Snap ()
site =
    ifTop (writeBS "hello world") <|>
    route [ ("foo", writeBS "bar")
          ]
