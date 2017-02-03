{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Battle
( Auction(auc, buyout, item, owner, quantity)
, Auctions(auctions)
, AuctionFile(lastModified, url)
, AuctionFiles(files)
, getAuction
, getAuctionFiles
, getNewAuctions
, getNewCurrentAuctions
, getSharonsAuctions
, getUndercuttingAuctions
, love
, shouldNotify
) where


import Auction
import ListMap
import Secrets

import Control.Exception (throwIO)
import Control.Concurrent.MVar
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import Data.Foldable (any)
import Data.List as L
import Data.Map as M
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import GHC.Generics
import Network.HTTP.Req

auctionUrl :: B.ByteString
auctionUrl = B.pack "https://us.api.battle.net/wow/auction/data/twisting-nether?locale=en_US"
-- auctionUrl = B.pack "https://us.api.battle.net/wow/auction/data/tichondrius?locale=en_US"

love :: String
love = "Akelia"

data Auction = Auction { auc :: Int
                       , buyout :: Int
                       , item :: Int
                       , owner :: String
                       , quantity :: Int } deriving (Generic, Show)
instance FromJSON Auction

-- TODO get actual item data
data Auctions = Auctions { auctions :: [Auction] } deriving (Generic, Show)
instance FromJSON Auctions

data AuctionFile = AuctionFile { lastModified :: Int
                               , url :: String }
                               deriving (Generic, Show)
instance FromJSON AuctionFile

data AuctionFiles = AuctionFiles { files :: [AuctionFile] }
    deriving (Generic, Show)
instance FromJSON AuctionFiles

-- TODO get rid of this
instance MonadHttp IO where
    handleHttpException = throwIO

getAuction :: B.ByteString -> IO [Auction]
getAuction s = do
    let parseResult = parseUrlHttp s
    case parseResult of
        Just (url', opts) ->
            auctions <$> responseBody <$>
                req GET url' NoReqBody jsonResponse opts
        _ -> return [] -- TODO log something

getAuctionFiles :: IO AuctionFiles
getAuctionFiles = do
    let Just (url', _) = parseUrlHttps auctionUrl
        opts = "locale" =: ("en_US" :: Text) <>
               "apikey" =: (pack apiKey)
    rsp <- req GET url' NoReqBody jsonResponse opts
    return $ responseBody rsp

getNewAuctions :: [AuctionFile] -> IO [[Auction]]
getNewAuctions newFiles = mapM (getAuction . B.pack . url) newFiles

getNewCurrentAuctions :: ListMap Int AuctionMetadata -> ListMap Int AuctionMetadata
    -> ListMap Int AuctionMetadata
getNewCurrentAuctions sharonsAuctions undercuttingAuctions =
    M.mapWithKey (\k v -> M.findWithDefault [] k undercuttingAuctions ++ v) sharonsAuctions

getNewFiles :: Int -> IO [AuctionFile]
getNewFiles minTime = L.filter (\f -> minTime < lastModified f) <$> files
          <$> getAuctionFiles

getSharonsAuctions :: [[Auction]] -> ListMap Int AuctionMetadata
getSharonsAuctions newAuctions =
    let isSharons = (love ==) . owner
    in toSortedMeta isSharons newAuctions

getUndercuttingAuctions :: [[Auction]] -> ListMap Int AuctionMetadata -> ListMap Int AuctionMetadata
getUndercuttingAuctions newAuctions sharonsAuctions =
    let sharonsPrices :: M.Map Int Double
        sharonsPrices = M.map maximum $ 
            M.map (L.map meta_pricePerItem) sharonsAuctions
        isUndercutting :: Auction -> Bool
        isUndercutting a =
            let item' = item a
                buyout' = buyout a
                quantity' = quantity a
            in (M.member item' sharonsPrices) &&
                   ((toPricePerItem buyout' quantity') <
                   (sharonsPrices M.! item'))
    in toSortedMeta isUndercutting newAuctions

{-
 - We should notify if there is any undercutting auction that was not seen
 - previously
 -}
shouldNotify :: ListMap Int AuctionMetadata -> ListMap Int AuctionMetadata -> Bool
shouldNotify prevAuctions undercuttingAuctions =
    let itemTest :: Int -> [AuctionMetadata] -> Bool
        itemTest k v = (not $ M.member k prevAuctions) ||
            (hasNew (prevAuctions M.! k) v)
        auc_same :: AuctionMetadata -> AuctionMetadata -> Bool
        auc_same a b = meta_auc a == meta_auc b
        hasNew :: [AuctionMetadata] -> [AuctionMetadata] -> Bool
        hasNew a b = not $ all (\b0 -> any (auc_same b0) a) b
    in or $ M.mapWithKey itemTest undercuttingAuctions

toPricePerItem :: Int -> Int -> Double
toPricePerItem buyout' quantity' =
    (fromIntegral buyout') / (fromIntegral quantity')

toMetadata :: Auction -> AuctionMetadata
toMetadata Auction{ auc=auc'
                  , buyout=buyout'
                  , item=item'
                  , owner=owner'
                  , quantity=quantity' } =
    AuctionMetadata { meta_auc=auc'
                    , meta_buyout=buyout'
                    , meta_item=item'
                    , meta_owner=owner'
                    , meta_pricePerItem=toPricePerItem buyout' quantity'
                    , meta_quantity=quantity' }

toSortedMeta :: (Auction -> Bool) -> [[Auction]] -> ListMap Int AuctionMetadata
toSortedMeta filterFun newAuctions = M.map (L.sortOn meta_pricePerItem) $
    L.foldr (\a m -> add (meta_item a) a m) M.empty $
    L.map toMetadata $ concat $ L.map (L.filter filterFun) newAuctions

update :: AuctionMonadT IO Bool
update = do
    currentAuctionsM <- getCurrentAuctions
    lastAuctionTimeM <- getLastAuctionTime

    lift $ do
        prevAuctions <- readMVar currentAuctionsM
        lastAuctionTime <- readMVar lastAuctionTimeM

        newFiles <- getNewFiles lastAuctionTime
        newAuctions <- getNewAuctions newFiles
        let sharonsAuctions = getSharonsAuctions newAuctions
            undercuttingAuctions = getUndercuttingAuctions newAuctions sharonsAuctions
            newCurrentAuctions = getNewCurrentAuctions sharonsAuctions undercuttingAuctions

        swapMVar currentAuctionsM $! newCurrentAuctions
        swapMVar lastAuctionTimeM $! L.maximum $ L.map lastModified newFiles
        return $! shouldNotify prevAuctions undercuttingAuctions
