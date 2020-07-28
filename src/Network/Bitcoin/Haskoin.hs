module Network.Bitcoin.Haskoin
    ( transactionOutputAddress
    , transactionInputAddress
    , getTransaction
    , getTransactionOutput
    , outpointAddress
    , importAddress

    , -- * Utility functions
      addressToHex
    , decodeHexTx
    , hexTxHash
    , hexToAddress
    , transactionIdToTxHash

    , -- * network-bitcoin reexports
      getClient
    , Client

    , -- * haskoin Rexports
      outputAddress
    , inputAddress
    ) where

import           Control.Monad          ((<=<))
import qualified Data.ByteString.Base16 as B16
import           Data.Maybe             (fromMaybe)
import           Data.Serialize         (decode)
import           Data.Text.Encoding     as E

import           Haskoin.Address        (Address, addrToText, inputAddress,
                                         outputAddress, scriptToAddressBS,
                                         textToAddr)
import           Haskoin.Constants      (Network)
import           Haskoin.Script         (decodeInputBS)
import           Haskoin.Transaction    (OutPoint (..), Tx (..), TxHash, TxIn,
                                         TxOut, hexToTxHash, scriptInput,
                                         scriptOutput, txHashToHex)
import           Network.Bitcoin        (Client, RawTransaction, TransactionID,
                                         getClient, getRawTransaction)
import qualified Network.Bitcoin        as B


transactionOutputAddress :: TxOut -> Either String Address
transactionOutputAddress = scriptToAddressBS . scriptOutput


transactionInputAddress :: Network -> TxIn -> Either String Address
transactionInputAddress net = maybe (Left "could not decode address") Right . inputAddress <=< decodeInputBS net . scriptInput


addressToHex :: Network -> Address -> B.Address
addressToHex net = fromMaybe (error "Address encoding error") . addrToText net


hexToAddress :: Network -> B.Address -> Address
hexToAddress net = fromMaybe (error "Unable to parse address") . textToAddr net


-- | TODO Catch bad decodes
decodeHexTx :: RawTransaction -> Tx
decodeHexTx = fromRight . decode . fst . B16.decode . E.encodeUtf8
  where
    fromRight (Right x) = x
    fromRight (Left e)  = error e


hexTxHash :: TxHash -> TransactionID
hexTxHash = txHashToHex


transactionIdToTxHash :: TransactionID -> TxHash
transactionIdToTxHash = fromMaybe (error "Unable to parse txid") . hexToTxHash


-- | TODO Catch errors from bitcoind
getTransaction :: Client -> TxHash -> IO Tx
getTransaction c hash = decodeHexTx <$> getRawTransaction c (hexTxHash hash)


getTransactionOutput :: Client -> OutPoint -> IO TxOut
getTransactionOutput cl (OutPoint hash i) = (!! fromIntegral i) . txOut <$> getTransaction cl hash


outpointAddress :: Client -> OutPoint -> IO (Either String Address)
outpointAddress c op = transactionOutputAddress <$> getTransactionOutput c op


importAddress :: Client -> Network -> Address -> Maybe B.Account -> Maybe Bool -> IO ()
importAddress client net addr = B.importAddress client (addressToHex net addr)
