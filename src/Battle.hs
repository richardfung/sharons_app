{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Battle
( Auction(buyout, item, owner, quantity)
, AuctionFile(lastModified, url)
, AuctionFiles(files)
, getAuction
, getAuctionFiles
, getNewAuctions
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

getNewAuctions :: AuctionMonadT IO Bool
getNewAuctions = do
    lastAuctionTime <- getLastAuctionTime
    currentAuctionsM <- getCurrentAuctions

    lift $ do
        prevAuctions <- readMVar currentAuctionsM
        minTime <- readMVar lastAuctionTime

        newFiles <- L.filter (\f -> minTime < lastModified f) <$> files
            <$> getAuctionFiles
        newAuctions <- mapM (getAuction . B.pack . url) newFiles
        let isSharons :: Auction -> Bool
            isSharons = (love ==) . owner
            toSortedMeta :: (Auction -> Bool) -> ListMap Int AuctionMetadata
            toSortedMeta filterFun = M.map (L.sortOn meta_pricePerItem) $
                L.foldr (\a m -> add (meta_item a) a m) M.empty $
                L.map toMetadata $ concat $ L.map (L.filter filterFun) newAuctions
            sharonsAuctions = toSortedMeta isSharons
            sharonsPrices :: M.Map Int Double
            sharonsPrices = M.map maximum $ 
                M.map (L.map meta_pricePerItem) sharonsAuctions
            undercuttingAuctions =
                let isUndercutting :: Auction -> Bool
                    isUndercutting a =
                        let item' = item a
                            buyout' = buyout a
                            quantity' = quantity a
                        in (M.member item' sharonsPrices) &&
                               ((toPricePerItem buyout' quantity') <
                               (sharonsPrices M.! item'))
                in toSortedMeta isUndercutting
            shouldNotify = any (any (\a -> all (\other -> meta_auc other /= meta_auc a)
                                    (prevAuctions M.! (meta_item a)))
                               ) undercuttingAuctions
            newCurrentAuctions = M.mapWithKey (\k v -> undercuttingAuctions M.! k ++ v)
                                              sharonsAuctions
        --
        -- set new current auctions
        swapMVar currentAuctionsM $ newCurrentAuctions

        -- set new lastAuctionTime
        swapMVar lastAuctionTime $ maximum $ L.map lastModified newFiles
        return shouldNotify

getAuctionFiles :: IO AuctionFiles
getAuctionFiles = do
    let Just (url', _) = parseUrlHttps auctionUrl
        opts = "locale" =: ("en_US" :: Text) <>
               "apikey" =: (pack apiKey)
    rsp <- req GET url' NoReqBody jsonResponse opts
    return $ responseBody rsp

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
