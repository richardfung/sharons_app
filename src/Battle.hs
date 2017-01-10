{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Battle
( AuctionFile
, getAuctionFile
) where

import Secrets

import Control.Exception (throwIO)
import Data.Aeson
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import GHC.Generics
import Network.HTTP.Req
import qualified Data.ByteString.Char8 as B

auctionUrl :: B.ByteString
auctionUrl = B.pack "https://us.api.battle.net/wow/auction/data/tichondrius?locale=en_US"

love :: String
love = "Akelia"

-- TODO get actual item data and maybe cache the per item price
data Auction = Auction { itemId :: Int
                       , owner :: String
                       , buyout :: Int
                       , quantity :: Int } deriving (Generic, Show)
instance FromJSON Auction

data Auctions = Auctions { auctions :: [Auction] } deriving (Generic, Show)
instance FromJSON Auctions

data AuctionFile = AuctionFile { url :: String
                               , lastModified :: Int }
                               deriving (Generic, Show)
instance FromJSON AuctionFile

data AuctionFiles = AuctionFiles { files :: [AuctionFile] }
    deriving (Generic, Show)
instance FromJSON AuctionFiles

instance MonadHttp IO where
    handleHttpException = throwIO -- TODO log or something

getAuctionFile :: IO AuctionFile
getAuctionFile = do
    let Just (url', _) = parseUrlHttps auctionUrl
        opts = "locale" =: ("en_US" :: Text) <>
               "apikey" =: (pack apiKey)
    rsp <- req GET url' NoReqBody jsonResponse opts
    return $ responseBody rsp
