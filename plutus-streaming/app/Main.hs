{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Cardano.Api (ChainPoint (ChainPoint, ChainPointAtGenesis), NetworkId (Mainnet, Testnet),
                    NetworkMagic (NetworkMagic), SlotNo (SlotNo))
import Cardano.Api.Extras ()
import Data.Aeson.Text qualified as Aeson
import Data.Text.Lazy qualified as TL
import Options.Applicative (Alternative ((<|>)), Parser, auto, execParser, flag', help, helper, info, long, metavar,
                            option, str, strOption, (<**>))
import Plutus.Streaming (StreamerEvent (Append, Revert), withSimpleStreamerEventStream)
import Streaming.Prelude qualified as S

--
-- Options parsing
--

data Options = Options
  { optionsSocketPath :: String,
    optionsNetworkId  :: NetworkId,
    optionsChainPoint :: ChainPoint
  }
  deriving (Show)

optionsParser :: Parser Options
optionsParser =
  Options
    <$> strOption (long "socket-path" <> help "Node socket path")
    <*> networkIdParser
    <*> chainPointParser

networkIdParser :: Parser NetworkId
networkIdParser =
  pMainnet' <|> fmap Testnet testnetMagicParser
  where
    pMainnet' :: Parser NetworkId
    pMainnet' =
      flag'
        Mainnet
        ( long "mainnet"
            <> help "Use the mainnet magic id."
        )

testnetMagicParser :: Parser NetworkMagic
testnetMagicParser =
  NetworkMagic
    <$> option
      auto
      ( long "testnet-magic"
          <> metavar "NATURAL"
          <> help "Specify a testnet magic id."
      )

chainPointParser :: Parser ChainPoint
chainPointParser =
  pure ChainPointAtGenesis
    <|> ( ChainPoint
            <$> option (SlotNo <$> auto) (long "slot-no" <> metavar "SLOT-NO")
            <*> option str (long "block-hash" <> metavar "BLOCK-HASH")
        )

--
-- Main
--

main :: IO ()
main = do
  Options {optionsSocketPath, optionsNetworkId, optionsChainPoint} <-
    execParser $ info (optionsParser <**> helper) mempty

  withSimpleStreamerEventStream
    optionsSocketPath
    optionsNetworkId
    optionsChainPoint
    $ S.stdoutLn
      . S.map
        ( \case
            Append cp _bim ->
              "Append block: " <> TL.unpack (Aeson.encodeToLazyText cp)
            Revert cp ->
              "Revert to point: " <> TL.unpack (Aeson.encodeToLazyText cp)
        )
