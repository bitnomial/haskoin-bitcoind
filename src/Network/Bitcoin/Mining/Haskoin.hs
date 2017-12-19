module Network.Bitcoin.Mining.Haskoin
  ( generate
  ) where

import           Data.Fixed                  (Fixed (MkFixed))
import           Data.Maybe                  (fromMaybe, maybe)
import           Data.Text                   (Text)
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import           Data.Word                   (Word64)
import           Network.Bitcoin.Haskoin     (addressToHex,
                                              transactionIdToTxHash)
import qualified Network.Bitcoin.Mining      as NBM
import qualified Network.Bitcoin.Types       as NBT
import           Network.Bitcoin.Wallet      (Client)
import qualified Network.Bitcoin.Wallet      as W
import           Network.Haskoin.Crypto      (addrToBase58, base58ToAddr)
import qualified Network.Haskoin.Crypto      as HSK
import           Network.Haskoin.Transaction (TxHash, hexToTxHash)


generate :: Client -> Int -> Maybe Int -> IO [TxHash]
generate client blocks maxTries = fmap transactionIdToTxHash <$> NBM.generate client blocks maxTries


generateToAddress :: Client -> Int -> HSK.Address -> Maybe Int -> IO [TxHash]
generateToAddress client blocks toAddr maxTries =
    fmap transactionIdToTxHash <$> NBM.generateToAddress client blocks (addressToHex toAddr) maxTries
