module Plutus.Contract.Test.ContractModel.MissingLovelace
  ( calculateDelta
  ) where

import Data.List as List
import Ledger.Ada qualified as Ada
import Ledger.Value (Value, noAdaValue)
import PlutusTx.Prelude qualified as P
import Wallet.Emulator (Wallet)

calculateDelta :: Value -> Value -> Value -> Wallet -> [(Wallet, [Value])] -> Value
calculateDelta initialDelta initialValue finalValue w txOutCosts =
  let
    deltas =
      let txOutCosts' = case List.uncons $ reverse $ filter ((== w) . fst) txOutCosts of
            Just ((_, vs), _) -> map Ada.fromValue vs
            _                 -> []
          otherWalletsTxOutCosts = concatMap (map Ada.fromValue . snd) $ txOutCosts
      in map P.abs $ concat
        [ [ P.abs val P.- P.abs wCost
          , P.abs val P.+ P.abs wCost ] | val <- [Ada.fromValue initialDelta, 0] ++ txOutCosts'
                                        , wCost <- txOutCosts' ++ otherWalletsTxOutCosts ]
    finalValueDelta = Ada.fromValue $ finalValue P.- initialValue
    missingDelta = if or [(P.abs finalValueDelta) `mod` d == 0 | d <- deltas, d /= 0] then
      let d = Ada.toValue finalValueDelta
          missingNonAda = noAdaValue initialDelta
      in d <> missingNonAda
      else initialDelta
    dlt = if Ada.fromValue initialDelta == finalValueDelta then initialDelta else missingDelta
  in dlt
