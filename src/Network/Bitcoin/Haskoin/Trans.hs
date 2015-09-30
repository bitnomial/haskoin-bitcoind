{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}

module Network.Bitcoin.Haskoin.Trans
    ( BitcoinT (..)
    , runBitcoinT

    , -- * Bitcoind operations
      getTransaction
    , getTransactionOutput
    , outpointAddress

    , -- * Reexports
      Tx (..)
    , TxHash
    , Address
    , TxOut (..)
    , TxIn (..)
    , MonadBase
    , liftBase

    , -- * Utilities
      withClient
    , withClientIO
    ) where

import           Control.Monad.Base          (MonadBase, liftBase)
import           Control.Monad.Reader        (MonadReader, ReaderT (..), ask)
import           Control.Monad.Trans         (MonadTrans (..))
-- import           Control.Monad.Trans.Control

import           Network.Bitcoin.Haskoin     (Client)
import qualified Network.Bitcoin.Haskoin     as B
import           Network.Haskoin.Crypto      (Address, TxHash)
import           Network.Haskoin.Transaction (OutPoint, Tx (..), TxIn (..),
                                              TxOut (..))


newtype BitcoinT m a = BitcoinT { unBitcoinT :: ReaderT Client m a }
    deriving (Functor, Applicative, Monad, MonadTrans, MonadReader Client)


instance MonadBase b m => MonadBase b (BitcoinT m) where
    liftBase = lift . liftBase


runBitcoinT :: Client -> BitcoinT m a -> m a
runBitcoinT c = flip runReaderT c . unBitcoinT
-- TODO Handle exceptions


withClient :: Monad m => (Client -> m a) -> BitcoinT m a
withClient f = do
    cl <- ask
    lift (f cl)


withClientIO :: MonadBase IO m => (Client -> IO a) -> BitcoinT m a
withClientIO f = do
    cl <- ask
    liftBase (f cl)


getTransaction :: MonadBase IO m => TxHash -> BitcoinT m Tx
getTransaction hash = withClientIO (`B.getTransaction` hash)


getTransactionOutput :: MonadBase IO m => OutPoint -> BitcoinT m TxOut
getTransactionOutput op = withClientIO (`B.getTransactionOutput` op)


outpointAddress :: MonadBase IO m => OutPoint -> BitcoinT m (Either String Address)
outpointAddress op = withClientIO (`B.outpointAddress` op)
