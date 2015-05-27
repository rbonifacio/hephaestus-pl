module Main where

import HplAssets.Hephaestus.IO
import FeatureModel.Types


-- This function take only one parameter, which is the name of the configuration to build and test (those under where keyword).
main = buildHpl fcUcmBpm
 where
  fcUcm = FeatureConfiguration
     $ Root
      (f { fId = "HephaestusPL" })
       [ 
         Root 
         (f { fId = "SPLAsset" }) [Leaf $ f { fId = "UseCase" }],
	 Root 
         (f { fId = "OutpuFormat" }) [Leaf $ f { fId = "UcmToXML" }]  
       ]
  fcBpm = FeatureConfiguration
     $ Root
      (f { fId = "HephaestusPL" })
       [ 
         Root 
         (f { fId = "SPLAsset" }) [Leaf $ f { fId = "BusinessProcess" }],
	 Root 
         (f { fId = "OutpuFormat" }) [Leaf $ f { fId = "BpmToXML" }]  
       ]
  fcUcmBpm = FeatureConfiguration
     $ Root
      (f { fId = "HephaestusPL" })
       [ 
         Root 
         (f { fId = "SPLAsset" }) [Leaf $ f { fId = "UseCase" }, Leaf $ f { fId = "BusinessProcess" }],
	 Root 
         (f { fId = "OutpuFormat" }) [Leaf $ f { fId = "UcmToXML" }, Leaf $ f { fId = "BpmToXML" }]  
       ]   
  fcReq = FeatureConfiguration
     $ Root
      (f { fId = "HephaestusPL" })
       [ 
         Root 
         (f { fId = "SPLAsset" }) [Leaf $ f { fId = "Requirement" }]  
       ]
  fcDtmc = FeatureConfiguration
     $ Root
      (f { fId = "HephaestusPL-Dtmc" })
       [ 
         Root 
         (f { fId = "SPLAsset" }) [Leaf $ f { fId = "DTMC" }]  
       ]
  fcUcm1 = FeatureConfiguration
     $ Root
      (f { fId = "HephaestusPL" })
       [ 
         Root 
         (f { fId = "SPLAsset" }) [Leaf $ f { fId = "UseCase" }]  
       ]
  fcComp = FeatureConfiguration
     $ Root
      (f { fId = "HephaestusPL" })
       [ 
         Root 
         (f { fId = "SPLAsset" }) [Leaf $ f { fId = "Code" }]  
       ]
  fc = FeatureConfiguration
     $ Root
      (f { fId = "HephaestusPL" })
       [ 
         Root 
         (f { fId = "SPLAsset" }) [Leaf $ f { fId = "Requirement" }, Leaf $ f { fId = "UseCase" }]  
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
