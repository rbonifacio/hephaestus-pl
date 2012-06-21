module HplDrivers.BuildHephaestus where

import HplAssets.Hephaestus.IO
import FeatureModel.Types

main = buildHpl fc
 where
  fc = FeatureConfiguration
     $ Leaf 
     $ f { fId = "Hephaestus" }

  -- To turn off warnings
  f = Feature {
       fId = undefined,
       fName = undefined,
       fType = undefined,
       groupType = undefined,
       properties = undefined
      }
