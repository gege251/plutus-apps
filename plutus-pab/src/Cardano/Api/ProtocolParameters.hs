module Cardano.Api.ProtocolParameters where

import Cardano.Api.Shelley (ProtocolParameters)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as BSL
import Data.Default (def)
import Ledger.ProtocolParameters ()

readProtocolParameters :: Maybe FilePath -> IO ProtocolParameters
readProtocolParameters = maybe (pure def) readPP
  where
    readPP path = do
      bs <- BSL.readFile path
      case eitherDecode bs of
        Left err -> error $ "Error reading protocol parameters JSON file: "
                         ++ show path ++ " (" ++ err ++ ")"
        Right params -> pure params
