module HplAssets.Hephaestus.MetaDataTypes where

data AssetMetaData
   = AssetMetaData {
       assetModuleType :: String, -- module name where is the definition of the Asset's data types
       assetModuleParser :: String, -- module name here is the parser of the asset
       assetModuleType' :: String, -- module name where is the definition of the data types SPLModel and InstanceModel
       assetModule :: String,
       assetModel :: String,
       assetSelector :: [(String, String) ],
       assetSelector' :: [(String, String) ],
       assetEmpty :: String,
       assetXType :: String,
       assetXFun :: String,
       assetVarProperty:: String,  -- information about instruction "let u = fromJust (findPropertyValue "usecase-model" ps)" in the main()
       assetNameProperty:: String,  -- 
       assetXFunParser :: String,  -- contain the name of function parser of the asset       
       assetVarParser :: String,
       assetParamParser :: String,
       assetLstTransf :: [ParserTransf]
     }

data ExportMetaData
   = ExportMetaData {
       exportModule :: String,
       exportXType :: String,
       exportXFun :: String,
       exportXExt :: String,
       exportSelector :: String
   }
   
type ParserTransf = (String, String, String, String, String) 
-- ParserTransf = (string nome transformação, tipo de dados da transformação, lista de argumentos, formato dos argumentos para o tipo de dados da transformação, condição)
-- for example:
-- ("selectScenarios", "SelectScenarios", "x", "x", "Success")
-- ("bindParameter", "BindParameter", "[x,y]", "x y", "Success") 
