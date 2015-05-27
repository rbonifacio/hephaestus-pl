module Main where

import HplAssets.Hephaestus.IO
import FeatureModel.Types

main = buildHpl fcDtmc
 where
  fcDtmc = FeatureConfiguration
     $ Root
      (f { fId = "HephaestusPL-Code" })
       [ 
         Root 
         (f { fId = "SPLAsset" }) [Leaf $ f { fId = "Code" }]  
       ]

  -- To turn off warnings
  f = Feature {
       fId = undefined,
       fName = undefined,
       fType = undefined,
       groupType = undefined,
       properties = undefined
      }
