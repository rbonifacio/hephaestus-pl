module Main where

import HplAssets.Hephaestus.IO
import FeatureModel.Types

main = buildHpl fcUcmBpm 
 where
  fcUcmBpm = FeatureConfiguration
     $ Root
      (f { fId = "HephaestusPL" })
       [ 
         Root 
         (f { fId = "SPLAsset" }) [Leaf $ f { fId = "UseCase" }, Leaf $ f { fId = "BusinessProcess" }],
	 Root 
         (f { fId = "OutpuFormat" }) [Leaf $ f { fId = "UcmToXML" }, Leaf $ f { fId = "BpmToXML" }]  
       ]   

--data FeatureTree = Leaf Feature | Root Feature [FeatureTree]

  -- To turn off warnings
  f = Feature {
       fId = undefined,
       fName = undefined,
       fType = undefined,
       groupType = undefined,
       properties = undefined
      }
