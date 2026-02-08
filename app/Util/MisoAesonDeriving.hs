-- TODO this is a workaround for the fact that `Miso.JSON` deriving for sum types is completely broken
module Util.MisoAesonDeriving where

import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Bifunctor (first)
import Miso (ms)
import Miso.Aeson qualified
import Miso.JSON qualified

newtype MisoAeson a = MisoAeson a

instance (Aeson.ToJSON a) => Miso.JSON.ToJSON (MisoAeson a) where
    toJSON =
        Miso.Aeson.aesonToJSON
            . Aeson.toJSON
            . \(MisoAeson x) -> x
instance (Aeson.FromJSON a) => Miso.JSON.FromJSON (MisoAeson a) where
    parseJSON =
        fmap MisoAeson
            . Miso.JSON.Parser
            . first ms
            . flip Aeson.parseEither ()
            . const
            . Aeson.parseJSON
            . Miso.Aeson.jsonToAeson
