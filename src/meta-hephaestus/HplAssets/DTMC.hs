module HplAssets.DTMC (
  transformDtmc,
  emptyDtmc
) where

import BasicTypes
import HplAssets.DTMC.Types

import FeatureModel.Types
import Data.Generics
import Data.List


emptyDtmc :: DtmcModel -> DtmcModel
emptyDtmc dtmcmodel = dtmcmodel { graphs = [] }

transformDtmc :: DtmcTransformation -> DtmcModel -> FeatureConfiguration -> DtmcModel -> DtmcModel
transformDtmc (SelectDTMC) _ _ product = product
