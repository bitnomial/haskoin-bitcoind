module Network.Bitcoin.Mining.Haskoin
  ( generate
  , generateToAddress
  ) where

import qualified Haskoin.Address         as HSK
import           Haskoin.Constants       (Network)
import           Haskoin.Transaction     (TxHash)
import           Network.Bitcoin.Haskoin (addressToHex, transactionIdToTxHash)
import qualified Network.Bitcoin.Mining  as NBM
import           Network.Bitcoin.Wallet  (Client)


generate :: Client -> Int -> Maybe Int -> IO [TxHash]
generate client blocks maxTries = fmap transactionIdToTxHash <$> NBM.generate client blocks maxTries


generateToAddress :: Client -> Network -> Int -> HSK.Address -> Maybe Int -> IO [TxHash]
generateToAddress client net blocks toAddr maxTries =
    fmap transactionIdToTxHash <$> NBM.generateToAddress client blocks (addressToHex net toAddr) maxTries
