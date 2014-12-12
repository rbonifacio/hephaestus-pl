module Main where

import HplAssets.Hephaestus.IO
import FeatureModel.Types

main = buildHpl fcDtmc
 where
  fcDtmc = FeatureConfiguration
     $ Root
      (f { fId = "HephaestusPL-Dtmc" })
       [ 
         Root 
         (f { fId = "SPLAsset" }) [Leaf $ f { fId = "DTMC" }],
	 Root 
         (f { fId = "OutputFormat" }) [
                                  Leaf $ f { fId = "DotToDot" }]  

       ]

  -- To turn off warnings
  f = Feature {
       fId = undefined,
       fName = undefined,
       fType = undefined,
       groupType = undefined,
       properties = undefined
      }
