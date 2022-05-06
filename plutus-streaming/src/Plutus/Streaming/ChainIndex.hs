{-# LANGUAGE GADTs          #-}
{-# LANGUAGE NamedFieldPuns #-}

module Plutus.Streaming.ChainIndex
  ( utxoState,
    utxoState',
    UtxoState,
    TxUtxoBalance,
  )
where

import Cardano.Api (AddressAny, AddressInEra (AddressInEra), Block (Block), BlockInMode (BlockInMode), CardanoMode,
                    ChainPoint (ChainPointAtGenesis), CtxTx, NetworkId (Mainnet), Tx (Tx), TxBody (TxBody),
                    TxBodyContent (TxBodyContent, txOuts), TxOut (TxOut), toAddressAny)
import Data.Set (Set)
import Ledger (TxIn, TxOut, TxOutRef)
import Ledger.Tx.CardanoAPI (FromCardanoError)
import Plutus.ChainIndex (TxUtxoBalance)
import Plutus.ChainIndex.Compatibility qualified
import Plutus.ChainIndex.Compatibility qualified as CI
import Plutus.ChainIndex.Tx (ChainIndexTx (_citxInputs), txOutsWithRef)
import Plutus.ChainIndex.TxUtxoBalance qualified as TxUtxoBalance
import Plutus.ChainIndex.UtxoState (UtxoIndex, UtxoState)
import Plutus.ChainIndex.UtxoState qualified as UtxoState
import Plutus.Contract.CardanoAPI qualified
import Plutus.Streaming (ChainSyncEvent (RollBackward, RollForward), SimpleChainSyncEvent,
                         withSimpleChainSyncEventStream)
import Streaming (Of, Stream)
import Streaming.Prelude qualified as S

utxoState ::
  Monad m =>
  Stream (Of SimpleChainSyncEvent) m r ->
  Stream (Of (UtxoState TxUtxoBalance)) m r
utxoState =
  S.map snd . utxoState'

utxoState' ::
  Monad m =>
  Stream (Of SimpleChainSyncEvent) m r ->
  Stream (Of (SimpleChainSyncEvent, UtxoState TxUtxoBalance)) m r
utxoState' =
  S.scanned step initial projection
  where
    step index (RollForward block _) =
      case CI.fromCardanoBlock block of
        Left err -> error ("FromCardanoError: " <> show err)
        Right txs ->
          let tip = CI.tipFromCardanoBlock block
              balance = TxUtxoBalance.fromBlock tip txs
           in case UtxoState.insert balance index of
                Left err ->
                  error (show err)
                Right (UtxoState.InsertUtxoSuccess newIndex _insertPosition) ->
                  newIndex
    step index (RollBackward cardanoPoint _) =
      let point = CI.fromCardanoPoint cardanoPoint
       in case TxUtxoBalance.rollback point index of
            Left err -> error (show err)
            Right (UtxoState.RollbackResult _newTip rolledBackIndex) ->
              rolledBackIndex

    initial :: UtxoIndex TxUtxoBalance
    initial = mempty

    projection = UtxoState.utxoState

--
-- Experimental stuff
--

data Some f = forall a. Some (f a)

f ::
  BlockInMode CardanoMode ->
  [Some (Cardano.Api.TxOut CtxTx)]
f (BlockInMode (Block _bh txs) _eim) =
  concatMap (\(Tx (TxBody TxBodyContent {txOuts}) _kws) -> map Some txOuts) txs

getTxOuts ::
  BlockInMode CardanoMode ->
  [AddressAny]
getTxOuts (BlockInMode (Block _bh txs) _eim) =
  foldMap go txs
  where
    go (Tx (TxBody TxBodyContent {txOuts}) _kws) =
      map (\(TxOut (AddressInEra _ addr) _ _) -> toAddressAny addr) txOuts

