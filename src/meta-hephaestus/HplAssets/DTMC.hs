module HplAssets.DTMC (
  transformDtmc,
  emptyDtmc
) where

import BasicTypes
import HplAssets.DTMC.Types

import FeatureModel.Types
import Data.Generics
import Data.List
import Data.Maybe


emptyDtmc :: DtmcModel -> DtmcModel
emptyDtmc dtmcmodel = dtmcmodel { dtmcs = [] }

transformDtmc :: DtmcTransformation -> DtmcModel -> FeatureConfiguration -> DtmcModel -> DtmcModel
transformDtmc (SelectDTMC ids) model features product = product { dtmcs = selected }
    where
        selected = (dtmcs product) ++ chosenDtmcs
        chosenDtmcs = filter ((`elem` ids) . dtmcId) $ dtmcs model
