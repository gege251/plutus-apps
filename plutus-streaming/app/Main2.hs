{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Cardano.Api qualified
import Cardano.Api.Extras ()
import Data.Aeson qualified
import Data.ByteString.Lazy.Char8 qualified
import Data.Set (Set)
import Ledger qualified
import Options.Applicative (Alternative ((<|>)), Parser, auto, execParser, flag', help, helper, info, long, metavar,
                            option, str, strOption, (<**>))
import Plutus.Streaming (withSimpleChainSyncEventStream)
import Streaming.Prelude qualified as S

--
-- Options parsing
--

data Options = Options
  { optionsSocketPath :: String,
    optionsNetworkId  :: Cardano.Api.NetworkId,
    optionsChainPoint :: Cardano.Api.ChainPoint
  }
  deriving (Show)

optionsParser :: Parser Options
optionsParser =
  Options
    <$> strOption (long "socket-path" <> help "Node socket path")
    <*> networkIdParser
    <*> chainPointParser

networkIdParser :: Parser Cardano.Api.NetworkId
networkIdParser =
  pMainnet' <|> fmap Cardano.Api.Testnet testnetMagicParser
  where
    pMainnet' :: Parser Cardano.Api.NetworkId
    pMainnet' =
      flag'
        Cardano.Api.Mainnet
        ( long "mainnet"
            <> help "Use the mainnet magic id."
        )

testnetMagicParser :: Parser Cardano.Api.NetworkMagic
testnetMagicParser =
  Cardano.Api.NetworkMagic
    <$> option
      auto
      ( long "testnet-magic"
          <> metavar "NATURAL"
          <> help "Specify a testnet magic id."
      )

chainPointParser :: Parser Cardano.Api.ChainPoint
chainPointParser =
  pure Cardano.Api.ChainPointAtGenesis
    <|> ( Cardano.Api.ChainPoint
            <$> option (Cardano.Api.SlotNo <$> auto) (long "slot-no" <> metavar "SLOT-NO")
            <*> option str (long "block-hash" <> metavar "BLOCK-HASH")
        )

--
-- Main
--

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

main :: IO ()
main = do
  Options {optionsSocketPath, optionsNetworkId, optionsChainPoint} <-
    execParser $ info (optionsParser <**> helper) mempty

  withSimpleChainSyncEventStream
    optionsSocketPath
    optionsNetworkId
    optionsChainPoint
    (S.mapM_ Data.ByteString.Lazy.Char8.putStrLn . S.map Data.Aeson.encode . S.map (fmap (map txInsAndOuts . transactions)))

