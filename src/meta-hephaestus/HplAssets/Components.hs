module HplAssets.Components (
  transformComponent,
  emptyComponent
) where

import BasicTypes
import HplAssets.ComponentModel.Types

-- *******************************************************
import HplProducts.TestTypes -- where is defined the data types SPLModel and InstanceModel
-- *******************************************************

emptyComponent :: ComponentModel -> ComponentModel
emptyComponent comp = []

transformComponent :: ComponentTransformation -> SPLModel -> InstanceModel -> InstanceModel
transformComponent (SelectComponents ids) spl product = 
  let scs = [(snd x, snd x) | x <- splMappings spl, fst x `elem` ids] 
      ics = components product
  in case scs of 
         [] -> product 
         otherwise -> product { components = ics ++ scs }
  
transformComponent (SelectAndMoveComponent i t) spl product = 
  let scs = [snd x | x <- splMappings spl, fst x == i] 
      ics = components product
  in case scs of 
         [] -> product
         otherwise -> product { components = ics ++ [ (c, t) | c <- scs] }


transformComponent (CreateBuildEntries e) spl product = 
  let es = buildEntries product
  in product { buildEntries = es ++ e }
	 

transformComponent (PreProcessor e) spl product = 
  let es = preProcessFiles product
  in product { preProcessFiles = es ++ e }
  


