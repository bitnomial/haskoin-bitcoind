{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UndecidableInstances       #-}

module Network.Bitcoin.Haskoin.Trans
    ( BitcoinT (..)
    , runBitcoinT
    , newBitcoindClient
    , getBitcoindClient
    , BitcoindClient (..)

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

import           Control.Monad.Base      (MonadBase, liftBase)
import           Control.Monad.IO.Class
import           Control.Monad.Reader    (MonadReader, ReaderT (..), ask)
import           Control.Monad.Trans     (MonadTrans (..))
import           Data.ByteString         (ByteString)

import           Haskoin.Address         (Address)
import           Haskoin.Constants       (Network)
import           Haskoin.Transaction     (OutPoint, Tx (..), TxHash, TxIn (..),
                                          TxOut (..))
import           Network.Bitcoin         (Account)
import           Network.Bitcoin.Haskoin (Client)
import qualified Network.Bitcoin.Haskoin as B


data BitcoindClient = BitcoindClient
    { bitcoindClient  :: Client
    , bitcoindNetwork :: Network
    }


newBitcoindClient :: Client -> Network -> BitcoindClient
newBitcoindClient = BitcoindClient


type URL = String
type User = ByteString
type Password = ByteString


getBitcoindClient :: URL -> User -> Password -> Network -> IO BitcoindClient
getBitcoindClient url user password net = do
    client <- B.getClient url user password
    return $ newBitcoindClient client net


-- TODO Add Network to Reader
newtype BitcoinT m a = BitcoinT { unBitcoinT :: ReaderT BitcoindClient m a }
    deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadReader BitcoindClient)


instance MonadBase b m => MonadBase b (BitcoinT m) where
    liftBase = lift . liftBase


runBitcoinT :: BitcoindClient -> BitcoinT m a -> m a
runBitcoinT c = flip runReaderT c . unBitcoinT
-- TODO Handle exceptions


withClient :: Monad m => (Client -> m a) -> BitcoinT m a
withClient f = ask >>= lift . f . bitcoindClient


withClientIO :: MonadIO m => (Client -> IO a) -> BitcoinT m a
withClientIO f = ask >>= liftIO . f . bitcoindClient


getTransaction :: MonadIO m => TxHash -> BitcoinT m Tx
getTransaction hash = withClientIO (`B.getTransaction` hash)


getTransactionOutput :: MonadIO m => OutPoint -> BitcoinT m TxOut
getTransactionOutput op = withClientIO (`B.getTransactionOutput` op)


outpointAddress :: MonadIO m => OutPoint -> BitcoinT m (Either String Address)
outpointAddress op = withClientIO (`B.outpointAddress` op)


importAddress' :: MonadIO m => Address -> Maybe Account -> Maybe Bool -> BitcoinT m ()
importAddress' addr macct mrescan = do
    BitcoindClient cl net <- ask
    liftIO $ B.importAddress cl net addr macct mrescan


-- TODO Accounts are being removed, remove the account field from network-bitcoind
importAddress :: MonadIO m => Address -> BitcoinT m ()
importAddress addr = importAddress' addr (Just "") (Just False)
