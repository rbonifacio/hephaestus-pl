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

transformReq :: DtmcTransformation -> DtmcModel -> FeatureConfiguration -> DtmcModel -> DtmcModel
transformReq (SelectDTMC id) _ _ product = product 
transformReq (EvaluateExpr id) _ _ product = product 
transformReq (SelectDTMC name value) spl _ _ product = product

