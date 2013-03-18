module HplAssets.Hephaestus (
  transformHpl,
  emptyHpl,
  ExportHephaestusDoc(..),
  exportHplToDoc
) where

import HplAssets.Hephaestus.MetaProgramming
import HplAssets.Hephaestus.MetaDataTypes
import HplAssets.Hephaestus.MetaData
import HplAssets.Hephaestus.Types
import FeatureModel.Types
import Language.Haskell.Syntax
import Language.Haskell.Pretty
import Prelude hiding (lookup)
import Data.Map (lookup)

transformHpl :: HephaestusTransformation -> HephaestusModel -> FeatureConfiguration -> HephaestusModel -> HephaestusModel
transformHpl t (HephaestusModel [base]) _ (HephaestusModel modules) = HephaestusModel (transformHpl' t modules)
 where
  transformHpl' SelectBaseProduct [] = [selectBaseProduct base]
  transformHpl' (SelectAsset a) [m] = [selectAsset a m]
  transformHpl' (SelectExport b) [m] = [selectExport b m]
  transformHpl' (BindProductName n) [m] = [bindProductName n m]
  transformHpl' RemoveProductMainFunction [m] = [removeProductMainFunction m]
  transformHpl' SelectCKParser [m] = [selectCKParser m]
  transformHpl' t _ = error ("Transformation " ++ show t ++ " not applicable.")  

emptyHpl :: HephaestusModel -> HephaestusModel
emptyHpl hplmodel= HephaestusModel []

exportHplToDoc:: FilePath -> HephaestusModel -> IO()
exportHplToDoc f (HephaestusModel [m]) =  writeFile f (prettyPrint m)


-- ----------------------------------------------------------
-- Transformations
-- ----------------------------------------------------------
              
--
-- Rename module name
--  from HplProducts.Base 
--    to HplProducts.Test
--               

selectBaseProduct::HsModule -> HsModule
selectBaseProduct = bindProductName "Test" 
  . removeImportDecl ("HplProducts.Base")

--
-- Set product name explicitly.
--
bindProductName :: String -> HsModule -> HsModule
bindProductName n = setModuleName ("HplProducts." ++ n)

--
-- Select a specific asset.
-- Carry out all metaprogramming-based transformations.
-- Use metadata about the assets to this end.
--

selectAsset :: String -> HsModule -> HsModule
selectAsset n
  = addUpdateCase "transform" xfun xtype [selName',"id"] selName
  . initializeFieldWithFun "InstanceModel" selName selName' empty
  . addImportDecl ("HplAssets." ++ mod)
  . addImportDecl ("HplAssets." ++ modtype)
  . addImportDecl ("HplAssets." ++ modParser)  
  . addLetInstruction "main" "targetDir" "findPropertyValue"  xVarProperty xNameProperty      
  . addGeneratorInstruction "main" "parseInstanceModel" xVarParser xfunParser xParamParser 
  . initializeField "SPLModel" selName' xVarParser 
  . addField "InstanceModel" sel
  . addField "SPLModel" sel'
  . addConstructor "TransformationModel" xtype "UndefinedTransformation"
  . addCaseList "xml2Transformation" xtype lstTransf
 where
  metadata = maybe (error ("Missing metadata for " ++ n ++ ".")) id $ lookup n assetMetaData
  mod       = assetModule metadata
  modParser = assetModuleParser metadata
  modtype   = assetModuleType metadata
  sel       = assetSelector metadata
  sel'      = assetSelector' metadata
  selName   = fst $ head $ sel
  selName'  = fst $ head $ sel'
  empty     = assetEmpty metadata
  xfun      = assetXFun metadata
  xtype     = assetXType metadata
  xVarProperty   = assetVarProperty metadata
  xNameProperty  = assetNameProperty metadata
  xfunParser     = assetXFunParser metadata  
  xVarParser     = assetVarParser metadata
  xParamParser   = assetParamParser metadata
  lstTransf = assetLstTransf metadata
   

-- ??? What is this ???
 
addCaseList :: String -> String -> [ParserTransf] -> HsModule -> HsModule
addCaseList dataName typeTransf [(stTran, dtTran, peTran, pDtTran, condSuc)]    = addCase3 dataName typeTransf stTran dtTran peTran pDtTran condSuc
addCaseList dataName typeTransf ((stTran, dtTran, peTran, pDtTran, condSuc):xs) = addCase3 dataName typeTransf stTran dtTran peTran pDtTran condSuc . addCaseList dataName typeTransf xs
  
    
-- 
-- Select export functionality
-- 
selectExport :: String -> HsModule -> HsModule
selectExport n 
  = addCase2 "export" xfun xtype xext sel
  . addImportDecl ("HplAssets." ++ mod)
  . addConstructorWithoutArgs "ExportModel" xtype "UndefinedExport"
  . addListElem "lstExport" xtype 
 where
  metadata = maybe (error ("Missing metadata for " ++ n ++ ".")) id $ lookup n exportMetaData
  mod = exportModule metadata
  xtype = exportXType metadata
  xfun  = exportXFun metadata
  xext  = exportXExt metadata
  sel  = exportSelector metadata
  

-- insert CK parser instructions into Hephaestus's instance
selectCKParser::HsModule -> HsModule
selectCKParser
 = addLetInstruction "main" "iModel" "findPropertyValue"  "cModel"  "configuration-model"  
 . addGeneratorInstruction "main" "parseFeatureModel" "cm" "parseConfigurationKnowledge" "(ns ckSchema) (snd cModel)" 
 . addLetInstruction' "main" "product" 
      

-- remove the "main" function of the Hephaestus's instance M1 module       
removeProductMainFunction :: HsModule -> HsModule
removeProductMainFunction = removeFunction "main" " "
                          . removeFunction "main" "funcBody"               
