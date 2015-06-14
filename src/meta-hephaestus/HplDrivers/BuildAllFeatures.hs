module Main where

import HplAssets.Hephaestus.IO
import FeatureModel.Types


main = buildHpl fcAllIn
 where
  fcAllIn = FeatureConfiguration
     $ Root
      (f { fId = "HephaestusPL_AllIn" })
       [ 
         Root 
         (f { fId = "SPLAsset" }) [
                                  Leaf $ f { fId = "UseCase" }, 
                                  Leaf $ f { fId = "BusinessProcess" }, 
                                  Leaf $ f { fId = "DTMC" },
                                  Leaf $ f { fId = "Code" },
                                  Leaf $ f { fId = "Requirement" },
                                  Leaf $ f { fId = "Cloud" }],
	 Root 
         (f { fId = "OutpuFormat" }) [
                                  Leaf $ f { fId = "UcmToXML" },
                                  Leaf $ f { fId = "UcmToLatex"},
                                  Leaf $ f { fId = "ReqToLatex"},
                                  Leaf $ f { fId = "BpmToXML" }]  
       ]   
  
  -- To turn off warnings
  f = Feature {
       fId = undefined,
       fName = undefined,
       fType = undefined,
       groupType = undefined,
       properties = undefined
      }
