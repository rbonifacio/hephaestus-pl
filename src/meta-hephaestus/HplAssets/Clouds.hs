module HplAssets.Clouds (
  transformCloud,
  emptyCloud
) where

import BasicTypes
import HplAssets.Cloud.Types
import FeatureModel.Types

import Data.Generics
import Data.List

emptyCloud :: CloudModel -> CloudModel
emptyCloud cloudmodel = cloudmodel { clouds = [] }

transformCloud :: CloudTransformation -> CloudModel -> FeatureConfiguration -> CloudModel -> CloudModel
transformCloud (SelectAllClouds) spl _ product = spl                                 

transformCloud (SelectClouds ids) spl _ product = product {clouds = cs}
 where
   selected = [c | c <- (clouds spl) , (cloudId c) `elem` ids]
   cs = nub $ (clouds product) ++ selected   
   
transformReq (RemoveClouds ids) spl _ product = product { clouds = cs}  
 where 
  cs = [c | c <- (clouds product), not ((cloudId c) `elem` ids)]
