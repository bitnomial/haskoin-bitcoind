module Network.Bitcoin.Haskoin
    ( outputScriptAddress
    , inputScriptAddress
    , transactionOutputAddress
    , transactionInputAddress
    , getTransaction
    , getTransactionOutput
    , outpointAddress

    -- * Utility functions
    , decodeHexTx
    , hexTxHash

    -- * network-bitcoin reexports
    , getClient
    ) where

import           Control.Monad               (join)

import           Data.ByteString             as BS
import           Data.ByteString.Base16      as B16
import           Data.Text.Encoding          as E

import           Network.Bitcoin             (Client, RawTransaction,
                                              TransactionID, getClient,
                                              getRawTransaction)
import qualified Network.Bitcoin             as BTC
import           Network.Haskoin.Crypto
import           Network.Haskoin.Script
import           Network.Haskoin.Transaction
import           Network.Haskoin.Util


outputScriptAddress :: ScriptOutput -> Either String Address
outputScriptAddress (PayPKHash addr) = Right addr
outputScriptAddress (PayScriptHash addr) = Right addr
outputScriptAddress so = Left $ "Bad Script: " ++ show so


inputScriptAddress :: ScriptInput -> Either String Address
inputScriptAddress (RegularInput (SpendPKHash _ key)) = Right (pubKeyAddr key)
inputScriptAddress (ScriptHashInput _ rdm) = Right (scriptAddr rdm)
inputScriptAddress so = Left $ "Bad Script: " ++ show so


transactionOutputAddress :: TxOut -> Either String Address
transactionOutputAddress = join . fmap outputScriptAddress . decodeOutputBS . scriptOutput


transactionInputAddress :: TxIn -> Either String Address
transactionInputAddress = join . fmap inputScriptAddress . decodeInputBS . scriptInput

-- | TODO Catch bad decodes
decodeHexTx :: RawTransaction -> Tx
decodeHexTx = decode' . fst . B16.decode . E.encodeUtf8


hexTxHash :: TxHash -> TransactionID
hexTxHash = E.decodeUtf8 . B16.encode . BS.reverse . encode'


-- | TODO Catch errors from bitcoind
getTransaction :: Client -> TxHash -> IO Tx
getTransaction c hash = decodeHexTx <$> getRawTransaction c (hexTxHash hash)


getTransactionOutput :: Client -> OutPoint -> IO TxOut
getTransactionOutput cl (OutPoint hash i) = do
    tx <- getTransaction cl hash
    let txos = txOut tx
    return $ txos !! fromIntegral i


outpointAddress :: Client -> OutPoint -> IO (Either String Address)
outpointAddress c op = transactionOutputAddress <$> getTransactionOutput c op
