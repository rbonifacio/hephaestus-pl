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
transformDtmc (SelectDTMC id) _ _ product = product 
transformDtmc (EvaluateExpr id) _ _ product = product 
transformDtmc (BindParameterDTMC name value)  _ _ product = product

