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

import           Control.Monad               ((<=<))
import qualified Data.ByteString.Base16      as B16
import           Data.Maybe                  (fromMaybe)
import           Data.Serialize              (decode)
import           Data.Text.Encoding          as E

import           Network.Bitcoin             (Client, RawTransaction,
                                              TransactionID, getClient,
                                              getRawTransaction)
import qualified Network.Bitcoin             as B
import           Network.Haskoin.Crypto      (Address, addrToBase58,
                                              base58ToAddr)
import           Network.Haskoin.Script      (decodeInputBS, decodeOutputBS,
                                              inputAddress, outputAddress)
import           Network.Haskoin.Transaction (OutPoint (..), Tx (..), TxHash,
                                              TxIn, TxOut, hexToTxHash,
                                              scriptInput, scriptOutput,
                                              txHashToHex)


transactionOutputAddress :: TxOut -> Either String Address
transactionOutputAddress = outputAddress <=< decodeOutputBS . scriptOutput


transactionInputAddress :: TxIn -> Either String Address
transactionInputAddress = inputAddress <=< decodeInputBS . scriptInput


addressToHex :: Address -> B.Address
addressToHex = decodeUtf8 . addrToBase58


hexToAddress :: B.Address -> Address
hexToAddress = fromMaybe (error "Unable to parse address") . base58ToAddr . encodeUtf8


-- | TODO Catch bad decodes
decodeHexTx :: RawTransaction -> Tx
decodeHexTx = fromRight . decode . fst . B16.decode . E.encodeUtf8
  where
    fromRight (Right x) = x
    fromRight (Left e)  = error e


hexTxHash :: TxHash -> TransactionID
hexTxHash = E.decodeUtf8 . txHashToHex


transactionIdToTxHash :: TransactionID -> TxHash
transactionIdToTxHash = fromMaybe (error "Unable to parse txid") . hexToTxHash . encodeUtf8


-- | TODO Catch errors from bitcoind
getTransaction :: Client -> TxHash -> IO Tx
getTransaction c hash = decodeHexTx <$> getRawTransaction c (hexTxHash hash)


getTransactionOutput :: Client -> OutPoint -> IO TxOut
getTransactionOutput cl (OutPoint hash i) = (!! fromIntegral i) . txOut <$> getTransaction cl hash


outpointAddress :: Client -> OutPoint -> IO (Either String Address)
outpointAddress c op = transactionOutputAddress <$> getTransactionOutput c op


importAddress :: Client -> Address -> Maybe B.Account -> Maybe Bool -> IO ()
importAddress client addr = B.importAddress client (E.decodeUtf8 $ addrToBase58 addr)
