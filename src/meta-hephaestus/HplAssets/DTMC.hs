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


import Data.FDTMC (FeatureSelection,
                   resolve)


emptyDtmc :: DtmcModel -> DtmcModel
emptyDtmc dtmcmodel = dtmcmodel { dtmcs = [] }


transformDtmc :: DtmcTransformation -> DtmcModel -> FeatureConfiguration -> DtmcModel -> DtmcModel
transformDtmc (SelectDTMC ids) model features product = product { dtmcs = selected }
    where
        selected = (dtmcs product) ++ map (resolve' features) chosenDtmcs
        chosenDtmcs = filter ((`elem` ids) . dtmcId) $ dtmcs model

transformDtmc (AppendDTMC x y) model features product = model

transformDtmc (ComposeDTMC x y z) model features product = model




resolve' :: FeatureConfiguration -> Dtmc -> Dtmc
resolve' features dtmc = dtmc { chain = resolve (chain dtmc) featureVars }
    where
        featureVars = translateFeatureConfiguration features


translateFeatureConfiguration :: FeatureConfiguration -> FeatureSelection
translateFeatureConfiguration config = map (head . fId) flatFeatures
    where
        flatFeatures = map fnode (flatten $ fcTree config)
