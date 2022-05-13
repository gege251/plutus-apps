{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

module Plutus.Streaming.Transactions (transactions, txInsAndOuts, txInsAndOuts', datums) where

import Cardano.Api qualified
import Data.Set (Set)
import Data.Set qualified as Set
import Ledger qualified

transactions ::
  Cardano.Api.BlockInMode Cardano.Api.CardanoMode ->
  [Ledger.CardanoTx]
transactions (Cardano.Api.BlockInMode (Cardano.Api.Block _ txs) eim) =
  map (\tx -> Ledger.CardanoApiTx (workaround (Ledger.SomeTx tx) eim)) txs

-- https://github.com/input-output-hk/cardano-node/pull/3665
workaround ::
  (Cardano.Api.IsCardanoEra era => Cardano.Api.EraInMode era Cardano.Api.CardanoMode -> a) ->
  Cardano.Api.EraInMode era Cardano.Api.CardanoMode ->
  a
workaround k Cardano.Api.ByronEraInCardanoMode   = k Cardano.Api.ByronEraInCardanoMode
workaround k Cardano.Api.ShelleyEraInCardanoMode = k Cardano.Api.ShelleyEraInCardanoMode
workaround k Cardano.Api.AllegraEraInCardanoMode = k Cardano.Api.AllegraEraInCardanoMode
workaround k Cardano.Api.MaryEraInCardanoMode    = k Cardano.Api.MaryEraInCardanoMode
workaround k Cardano.Api.AlonzoEraInCardanoMode  = k Cardano.Api.AlonzoEraInCardanoMode

txInsAndOuts ::
  Ledger.CardanoTx ->
  (Set Ledger.TxIn, [(Ledger.TxOut, Ledger.TxOutRef)])
txInsAndOuts tx =
  (Ledger.getCardanoTxInputs tx, Ledger.getCardanoTxOutRefs tx)

txInsAndOuts' ::
  Ledger.CardanoTx ->
  (Ledger.TxId, [Ledger.TxOutRef], [Ledger.TxOutRef])
txInsAndOuts' tx = (txId, inRefs, outRefs)
  where
    txId = Ledger.getCardanoTxId tx
    inRefs = map Ledger.txInRef $ Set.toList $ Ledger.getCardanoTxInputs tx
    outRefs = map snd $ Ledger.getCardanoTxOutRefs tx

datums ::
  Ledger.CardanoTx ->
  [(Ledger.DatumHash, Ledger.Datum)]
datums tx = do
  let txIns = Set.toList $ Ledger.getCardanoTxInputs tx
  (Ledger.TxIn _ (Just (Ledger.ConsumeScriptAddress _validator _redeemer datum))) <- txIns
  pure (Ledger.datumHash datum, datum)
