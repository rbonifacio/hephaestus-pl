module HplAssets.Components (
  transformComponent,
  emptyComponent
) where


import FeatureModel.Types

import BasicTypes

import HplAssets.ComponentModel.Types

emptyComponent :: ComponentModel -> ComponentModel
emptyComponent cm = cm { components = [] } 

transformComponent :: ComponentTransformation -> ComponentModel -> FeatureConfiguration -> ComponentModel -> ComponentModel
transformComponent (SelectComponents ids) spl _ product =
  let 
    scs = [(snd x, snd x) | x <- components spl, fst x `elem` ids] 
    pcs = components product 
  in product { components = pcs ++ scs}
  
transformComponent (SelectAndMoveComponent i t) spl _ product = 
  let 
    scs = [snd x | x <- components spl, fst x == i] 
    pcs = components product
  in product { components = pcs ++ [ (c, t) | c <- scs] }


-- transformComponent (CreateBuildEntries e) spl _ product = 
--   let es = buildEntries product
--   in product { buildEntries = es ++ e }
	 

-- transformComponent (PreProcessor e) spl product = 
--   let es = preProcessFiles product
--   in product { preProcessFiles = es ++ e }
  