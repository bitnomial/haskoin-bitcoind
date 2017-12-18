module Network.Bitcoin.Wallet.Haskoin
    ( sendToAddress
    ) where

import           Data.Fixed                  (Fixed (MkFixed))
import           Data.Maybe                  (fromMaybe, maybe)
import           Data.Text                   (Text)
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import           Data.Word                   (Word64)
import qualified Network.Bitcoin.Types       as NBT
import           Network.Bitcoin.Wallet      (Client)
import qualified Network.Bitcoin.Wallet      as W
import           Network.Haskoin.Crypto      (addrToBase58, base58ToAddr)
import qualified Network.Haskoin.Crypto      as HSK
import           Network.Haskoin.Transaction (TxHash, hexToTxHash)


type Satoshi = Word64


sendToAddress :: Client -> HSK.Address -> Satoshi -> Maybe Text -> Maybe Text -> IO TxHash
sendToAddress client addr sat cmt cmtTo =
    hexStringToTxHash <$> W.sendToAddress client (addrToHexString addr) (satToBTC sat) cmt cmtTo


addrToHexString :: HSK.Address -> NBT.Address
addrToHexString = decodeUtf8 . addrToBase58


hexStringToTxHash :: NBT.TransactionID -> TxHash
hexStringToTxHash = fromMaybe (error "Unable to parse txid") . hexToTxHash . encodeUtf8


satToBTC :: Satoshi -> NBT.BTC
satToBTC = MkFixed . fromIntegral
