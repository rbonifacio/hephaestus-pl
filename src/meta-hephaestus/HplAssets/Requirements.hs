module HplAssets.Requirements (
  transformReq,
  emptyReq
) where

import BasicTypes
import HplAssets.ReqModel.Types

import FeatureModel.Types
import Data.Generics -- função everywhere
import Data.List -- to use function "nub"


emptyReq :: RequirementModel -> RequirementModel
emptyReq reqmodel = reqmodel { reqs = [] }

transformReq :: RequirementTransformation -> RequirementModel -> FeatureConfiguration -> RequirementModel -> RequirementModel
transformReq (SelectAllRequirements) spl _ product = spl 

transformReq (SelectRequirements ids) spl _ product = product { reqs =  rs }  
 where 
  selected = [r | r <- (reqs spl) , (reqId r) `elem` ids]
  rs = nub $ (reqs product) ++ selected

transformReq (RemoveRequirements ids) spl _ product = product { reqs = rs}  
 where 
  rs = [r | r <- (reqs product), not ((reqId r) `elem` ids)]

