{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UndecidableInstances       #-}

module Network.Bitcoin.Haskoin.Trans
    ( BitcoinT (..)
    , runBitcoinT

    , -- * Bitcoind operations
      getTransaction
    , getTransactionOutput
    , outpointAddress
    , importAddress

    , -- * Reexports
      Tx (..)
    , TxHash
    , Address
    , TxOut (..)
    , TxIn (..)
    , MonadBase
    , liftBase
    , MonadIO
    , liftIO

    , -- * Utilities
      withClient
    , withClientIO
    ) where

import           Control.Monad.Base          (MonadBase, liftBase)
import           Control.Monad.IO.Class
import           Control.Monad.Reader        (MonadReader, ReaderT (..), ask)
import           Control.Monad.Trans         (MonadTrans (..))

import           Network.Bitcoin             (Account)
import           Network.Bitcoin.Haskoin     (Client)
import qualified Network.Bitcoin.Haskoin     as B
import           Network.Haskoin.Address     (Address)
import           Network.Haskoin.Constants   (Network)
import           Network.Haskoin.Transaction (OutPoint, Tx (..), TxHash,
                                              TxIn (..), TxOut (..))


-- TODO Add Network to Reader
newtype BitcoinT m a = BitcoinT { unBitcoinT :: ReaderT Client m a }
    deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadReader Client)


instance MonadBase b m => MonadBase b (BitcoinT m) where
    liftBase = lift . liftBase


runBitcoinT :: Client -> BitcoinT m a -> m a
runBitcoinT c = flip runReaderT c . unBitcoinT
-- TODO Handle exceptions


withClient :: Monad m => (Client -> m a) -> BitcoinT m a
withClient f = do
    cl <- ask
    lift (f cl)


withClientIO :: MonadIO m => (Client -> IO a) -> BitcoinT m a
withClientIO f = do
    cl <- ask
    liftIO (f cl)


getTransaction :: MonadIO m => TxHash -> BitcoinT m Tx
getTransaction hash = withClientIO (`B.getTransaction` hash)


getTransactionOutput :: MonadIO m => OutPoint -> BitcoinT m TxOut
getTransactionOutput op = withClientIO (`B.getTransactionOutput` op)


outpointAddress :: MonadIO m => OutPoint -> BitcoinT m (Either String Address)
outpointAddress op = withClientIO (`B.outpointAddress` op)


importAddress' :: MonadIO m => Network -> Address -> Maybe Account -> Maybe Bool -> BitcoinT m ()
importAddress' net addr macct mrescan =
    withClientIO (\cl -> B.importAddress cl net addr macct mrescan)


importAddress :: MonadIO m => Network -> Address -> BitcoinT m ()
importAddress net addr = importAddress' net addr (Just "") (Just False)
