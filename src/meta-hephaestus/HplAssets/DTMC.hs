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
                  resolve,
                  append,
                  compose)


emptyDtmc :: DtmcModel -> DtmcModel
emptyDtmc dtmcmodel = dtmcmodel { dtmcs = [] }


transformDtmc :: DtmcTransformation -> DtmcModel -> FeatureConfiguration -> DtmcModel -> DtmcModel
transformDtmc (SelectDTMC ids) model features product = product { dtmcs = selected }
    where
        selected = (dtmcs product) ++ map (resolve' features) chosenDtmcs
        chosenDtmcs = filter ((`elem` ids) . dtmcId) $ dtmcs model

transformDtmc (AppendDTMC id point) model features product = product { dtmcs = appended }
    where
        appended =  map (append' chosenDtmc point ) (dtmcs product)
        chosenDtmc = fromMaybe (error "No fdtmc to append")  $ find (\dtmc -> dtmcId dtmc == id) $ dtmcs model

transformDtmc (ComposeDTMC id startpoint endpoint) model features product = product { dtmcs = composed }
    where
        composed =  map (compose' chosenDtmc startpoint endpoint) (dtmcs product)
        chosenDtmc = fromMaybe (error "No fdtmc to append")  $ find (\dtmc -> dtmcId dtmc == id) $ dtmcs model


resolve' :: FeatureConfiguration -> Dtmc -> Dtmc
resolve' features dtmc = dtmc { chain = resolve (chain dtmc) featureVars }
    where
        featureVars = translateFeatureConfiguration features

append' :: Dtmc -> Pointcut -> Dtmc -> Dtmc
append' fragment pointcut base = Dtmc { dtmcId = (dtmcId base), chain = append (chain base) (chain fragment) pointcut }

compose' :: Dtmc -> Pointcut -> Pointcut -> Dtmc -> Dtmc
compose' fragment startpoint endpoint base = Dtmc { dtmcId = (dtmcId base), chain = compose (chain base) (chain fragment) startpoint endpoint }


translateFeatureConfiguration :: FeatureConfiguration -> FeatureSelection
translateFeatureConfiguration config = map (head . fId) flatFeatures
    where
        flatFeatures = map fnode (flatten $ fcTree config)
