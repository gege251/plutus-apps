module Plutus.Streaming.ChainIndex
  ( utxoState,
    utxoState',
    UtxoState,
    TxUtxoBalance,
  )
where

import Plutus.ChainIndex (TxUtxoBalance)
import Plutus.ChainIndex.Compatibility qualified as CI
import Plutus.ChainIndex.TxUtxoBalance qualified as TxUtxoBalance
import Plutus.ChainIndex.UtxoState (UtxoIndex, UtxoState)
import Plutus.ChainIndex.UtxoState qualified as UtxoState
import Plutus.Streaming (SimpleStreamerEvent, StreamerEvent (Append, Revert))
import Streaming (Of, Stream)
import Streaming.Prelude qualified as S

utxoState ::
  Monad m =>
  Stream (Of SimpleStreamerEvent) m r ->
  Stream (Of (UtxoState TxUtxoBalance)) m r
utxoState =
  S.map snd . utxoState'

utxoState' ::
  Monad m =>
  Stream (Of SimpleStreamerEvent) m r ->
  Stream (Of (SimpleStreamerEvent, UtxoState TxUtxoBalance)) m r
utxoState' =
  S.scanned step initial projection
  where
    step index (Append _cp block) =
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
    step index (Revert cp) =
      let point = CI.fromCardanoPoint cp
       in case TxUtxoBalance.rollback point index of
            Left err -> error (show err)
            Right (UtxoState.RollbackResult _newTip rolledBackIndex) ->
              rolledBackIndex

    initial :: UtxoIndex TxUtxoBalance
    initial = mempty

    projection = UtxoState.utxoState
