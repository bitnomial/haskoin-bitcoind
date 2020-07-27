module Network.Bitcoin.Wallet.Haskoin
    ( sendToAddress
    , getNewAddress
    ) where

import           Data.Fixed              (Fixed (MkFixed))
import           Data.Text               (Text)
import           Data.Word               (Word64)
import qualified Haskoin.Address         as HSK
import           Haskoin.Constants       (Network)
import           Haskoin.Transaction     (TxHash)
import           Network.Bitcoin.Haskoin (addressToHex, hexToAddress,
                                          transactionIdToTxHash)
import qualified Network.Bitcoin.Types   as NBT
import           Network.Bitcoin.Wallet  (Client)
import qualified Network.Bitcoin.Wallet  as W


type Satoshi = Word64


sendToAddress :: Client -> Network -> HSK.Address -> Satoshi -> Maybe Text -> Maybe Text -> IO TxHash
sendToAddress client net addr sat cmt cmtTo =
    transactionIdToTxHash <$> W.sendToAddress client (addressToHex net addr) (satToBTC sat) cmt cmtTo


getNewAddress :: Client -> Network -> IO HSK.Address
getNewAddress client net = hexToAddress net <$> W.getNewAddress client Nothing


satToBTC :: Satoshi -> NBT.BTC
satToBTC = MkFixed . fromIntegral
