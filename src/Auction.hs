{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Auction
( AuctionMetadata( AuctionMetadata, meta_buyout, meta_item, meta_owner
                 , meta_pricePerItem, meta_quantity)
) where

import ListMap

import Control.Applicative ((<$>))
import Control.Concurrent.MVar
import Control.Monad.Reader (ReaderT, ask)

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

class (Monad m) => AuctionMonad m where
    getCurrentAuctions ::  m (MVar (ListMap Int AuctionMetadata))
    getLastAuctionTime ::  m (MVar Int)

newtype AuctionMonadT m a = AuctionMonadT (ReaderT (MVar (ListMap Int AuctionMetadata), MVar Int) m a) deriving (Applicative, Functor, Monad)
-- TODO figure out how to define this without GeneralizedNewtypeDeriving

instance (Monad m) => AuctionMonad (AuctionMonadT m) where
    getCurrentAuctions = AuctionMonadT $ fst <$> ask
    getLastAuctionTime = AuctionMonadT $ snd <$> ask
