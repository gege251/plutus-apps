{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Cardano.Api (ChainPoint (ChainPoint, ChainPointAtGenesis), NetworkId (Mainnet, Testnet),
                    NetworkMagic (NetworkMagic), SlotNo (SlotNo), ToJSON)
import Cardano.Api.Extras ()
import Data.Aeson qualified
import Data.ByteString.Lazy.Char8 qualified
import Options.Applicative (Alternative ((<|>)), Parser, auto, execParser, flag', help, helper, info, long, metavar,
                            option, str, strOption, (<**>))
import Plutus.Streaming (withSimpleStreamerEventStream)
import Plutus.Streaming.Transactions (transactions, txInsAndOuts')
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

printJson :: ToJSON a => S.Stream (S.Of a) IO r -> IO r
printJson = S.mapM_ Data.ByteString.Lazy.Char8.putStrLn . S.map Data.Aeson.encode

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
    (printJson . S.map (fmap (map txInsAndOuts' . transactions)))
