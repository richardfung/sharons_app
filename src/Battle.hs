{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Battle
( AuctionFile
, getAuctionFile
) where

import Secrets

import Control.Exception (throwIO)
import Control.Monad
import Data.Aeson
import Data.Maybe
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import GHC.Generics
import Network.HTTP.Req
import qualified Data.ByteString.Char8 as B

auctionUrl = B.pack "https://us.api.battle.net/wow/auction/data/tichondrius?locale=en_US"

data AuctionFile = AuctionFile { url :: String
                               , lastModified :: Int }
                               deriving (Generic, Show)
instance FromJSON AuctionFile

data AuctionFiles = AuctionFiles { files :: [AuctionFile] }
    deriving (Generic, Show)
instance FromJSON AuctionFiles

instance MonadHttp IO where
    handleHttpException = throwIO

getAuctionFile :: IO (Maybe AuctionFile)
getAuctionFile = do
    let Just (url, _) = parseUrlHttps auctionUrl
        opts = "locale" =: ("en_US" :: Text) <>
               "apikey" =: (pack apiKey)
    rsp <- req GET url NoReqBody jsonResponse opts
    let afs = responseBody rsp :: AuctionFiles
    return $ Just $ head $ files afs
