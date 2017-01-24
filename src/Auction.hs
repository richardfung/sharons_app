module Auction
( AuctionMetadata( AuctionMetadata, meta_buyout, meta_item, meta_owner
                 ,    meta_pricePerItem, meta_quantity)
) where

data AuctionMetadata = AuctionMetadata { meta_buyout :: Int
                                       , meta_item :: Int
                                       , meta_owner :: String
                                       , meta_pricePerItem :: Double
                                       , meta_quantity :: Int } deriving
                                       (Show)
instance Eq AuctionMetadata where
    a == b = (meta_pricePerItem a) == (meta_pricePerItem b)
instance Ord AuctionMetadata where
    compare a b = compare (meta_pricePerItem a) (meta_pricePerItem b)
