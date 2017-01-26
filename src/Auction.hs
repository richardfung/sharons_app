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

newtype AuctionMonadT m a = AuctionMonadT (ReaderT (MVar (ListMap Int AuctionMetadata), MVar Int) m a)

instance (Monad m) => AuctionMonad (AuctionMonadT m) where
    getCurrentAuctions = AuctionMonadT $ fst <$> ask
    getLastAuctionTime = AuctionMonadT $ snd <$> ask

instance (Monad m) => Functor (AuctionMonadT m) where
    fmap f (AuctionMonadT readerT) = AuctionMonadT $ fmap f readerT

instance (Monad m) => Applicative (AuctionMonadT m) where
    pure a = AuctionMonadT $ pure a
    (AuctionMonadT f) <*> (AuctionMonadT a) = AuctionMonadT $ f <*> a

instance (Monad m) => Monad (AuctionMonadT m) where
    (AuctionMonadT readerT) >>= f =
        let f' a = let AuctionMonadT r = f a in r
        in AuctionMonadT $ readerT >>= f'
