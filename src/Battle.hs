{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Battle
( AuctionFile
, getAuctionFiles
) where

import Secrets

import Control.Exception (throwIO)
import Control.Concurrent.MVar
import Data.Aeson
import Data.List as L
import Data.Map as M
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import GHC.Generics
import Network.HTTP.Req
import System.IO.Unsafe
import qualified Data.ByteString.Char8 as B

auctionUrl :: B.ByteString
auctionUrl = B.pack "https://us.api.battle.net/wow/auction/data/tichondrius?locale=en_US"

love :: String
love = "Akelia"

-- TODO add this to monad or something
-- First item in list will be Sharon's, rest will be auctions that are cheaper
-- this should be in sorted order by price per item
currentAuctions :: MVar (M.Map Int [AuctionMetadata])
currentAuctions = unsafePerformIO $ newMVar S.empty

-- TODO add this to monad or something
lastAuctionTime :: MVar Int
lastAuctionTime = unsafePerformIO $ newMVar 0

data Auction = Auction { buyout :: Int
                       , itemId :: Int
                       , owner :: String
                       , quantity :: Int } deriving (Generic, Show)
instance FromJSON Auction

-- TODO get actual item data
data AuctionMetadata = AuctionMetadata { meta_buyout :: Int
                                       , meta_itemId :: Int
                                       , meta_owner :: String
                                       , meta_pricePerItem :: Double
                                       , meta_quantity :: Int }
instance Ord AuctionMetadata where
    compare a b = compare (meta_pricePerItem a) (meta_pricePerItem b)

data Auctions = Auctions { auctions :: [Auction] } deriving (Generic, Show)
instance FromJSON Auctions

data AuctionFile = AuctionFile { lastModified :: Int
                               , url :: String }
                               deriving (Generic, Show)
instance FromJSON AuctionFile

data AuctionFiles = AuctionFiles { files :: [AuctionFile] }
    deriving (Generic, Show)
instance FromJSON AuctionFiles

instance MonadHttp IO where
    handleHttpException = throwIO -- TODO log or something

getAuction :: B.ByteString -> IO [Auction]
getAuction s = do
    let parseResult = parseUrlHttp s
    case parseResult of
        Just (url', opts) ->
            responseBody <$> req GET url' NoReqBody jsonResponse opts
        _ -> return [] -- TODO log something

getNewAuctions :: IO [Auction]
getNewAuctions = do
    minTime <- readMVar lastAuctionTime

    newFiles <- L.filter (\f -> minTime < lastModified f) <$> files
        <$> getAuctionFiles
    newAuctions <- mapM (getAuction . B.pack . url) newFiles
    let isSharons :: Auction -> Bool
        isSharons = (love ==) . owner
        toMeta :: [[Auction]] -> [AuctionMetadata]
        toMeta filterFun= L.map toMetadata $ concat $
            L.map (L.filter filterFun) newAuctions
        sharonsAuctions = toMeta isSharons
        isUndercutting :: Auction -> Bool
        undercuttingAuctions = toMeta

    swapMVar lastAuctionTime $ maximum $ L.map lastModified newFiles
    return []

getAuctionFiles :: IO AuctionFiles
getAuctionFiles = do
    let Just (url', _) = parseUrlHttps auctionUrl
        opts = "locale" =: ("en_US" :: Text) <>
               "apikey" =: (pack apiKey)
    rsp <- req GET url' NoReqBody jsonResponse opts
    return $ responseBody rsp

toMetadata :: Auction -> AuctionMetadata
toMetadata Auction{ buyout=buyout'
                  , itemId=itemId'
                  , owner=owner'
                  , quantity=quantity' } =
    AuctionMetadata { meta_buyout=buyout'
                    , meta_itemId=itemId'
                    , meta_owner=owner'
                    , meta_pricePerItem=(fromIntegral buyout') /
                        (fromIntegral quantity')
                    , meta_quantity=quantity' }
