module Main where

import HplAssets.Hephaestus.IO
import FeatureModel.Types

main = buildHpl fcCloud
 where
  fcCloud = FeatureConfiguration
     $ Root
      (f { fId = "HephaestusPL-Cloud" })
       [ 
         Root 
         (f { fId = "SPLAsset" }) [Leaf $ f { fId = "Cloud" }],
     Root 
         (f { fId = "OutputFormat" }) [
                                  Leaf $ f { fId = "DeploymentFile" }]  

       ]

  -- To turn off warnings
  f = Feature {
       fId = undefined,
       fName = undefined,
       fType = undefined,
       groupType = undefined,
       properties = undefined
      }
    