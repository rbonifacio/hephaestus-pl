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
transformHpl t (HephaestusModel [baseProduct1, baseProduct2]) _ (HephaestusModel modules) = HephaestusModel (transformHpl' t modules)
 where
  transformHpl' SelectBaseProduct [] = [selectBaseProductM1 baseProduct1, selectBaseProductM2 baseProduct2]
  transformHpl' (SelectAsset a) (m:mx) = [selectAssetM1 a m, selectAssetM2 a (head mx)]
  transformHpl' (SelectExport b) (m:mx) = [selectExportM1 b m, selectExportM2 b (head mx)]
  transformHpl' (BindProductName n) (m:mx) = [bindProductName n m, bindProductName (n++"Types") (head mx)]
  transformHpl' RemoveProductMainFunction (m:mx) = [removeProductMainFunction m, (head mx)]
  transformHpl' SelectCKParser (m:mx) = [selectCKParser m, (head mx)]
  transformHpl' t _ = error ("Transformation " ++ show t ++ " not applicable.")  

emptyHpl :: HephaestusModel -> HephaestusModel
emptyHpl hplmodel= HephaestusModel []

exportHplToDoc:: FilePath -> HephaestusModel -> IO()
exportHplToDoc f (HephaestusModel [p1, p2]) =  writeFile f (prettyPrint p1)


-- ----------------------------------------------------------
-- Transformations
-- ----------------------------------------------------------
              
--
-- Rename module name
--  from HplProducts.BaseProduct 
--    to HplProducts.Test
--               

selectBaseProductM1::HsModule -> HsModule
selectBaseProductM1 = bindProductName "Test" 
  . removeImportDecl ("HplProducts.BaseProductTypes")

selectBaseProductM2::HsModule -> HsModule
selectBaseProductM2 = bindProductName "TestTypes" 

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

-- M1 = main module of the Hephaestus's instance. For example, Test.hs, Hephaestus.hs 
selectAssetM1 :: String -> HsModule -> HsModule
selectAssetM1 n
  = addUpdateCase "transform" xfun xtype [sel',"id"] sel
  . initializeFieldWithFun "InstanceModel" sel sel' empty
  . addImportDecl ("HplAssets." ++ mod)
  . addImportDecl ("HplAssets." ++ modParser)  
  . addImportDecl ("HplProducts." ++ modtype')
  . addLetInstruction "main" "targetDir" "findPropertyValue"  xVarProperty xNameProperty      
  . addGeneratorInstruction "main" "parseInstanceModel" xVarParser xfunParser xParamParser 
  . initializeField "SPLModel" sel' xVarParser 
 where
  metadata = maybe (error ("Missing metadata for " ++ n ++ ".")) id $ lookup n assetMetaData
  mod    = assetModule metadata
  modParser = assetModuleParser metadata
  modtype' = assetModuleType' metadata
  sel'   = fst $ head $ assetSelector' metadata
  sel    = fst $ head $ assetSelector metadata
  empty  = assetEmpty metadata
  xfun   = assetXFun metadata
  xtype  = assetXType metadata
  xVarProperty   = assetVarProperty metadata
  xNameProperty  = assetNameProperty metadata
  xfunParser     = assetXFunParser metadata  
  xVarParser     = assetVarParser metadata
  xParamParser   = assetParamParser metadata
 
 
addCaseList :: String -> String -> [ParserTransf] -> HsModule -> HsModule
addCaseList dataName typeTransf [(stTran, dtTran, peTran, pDtTran, condSuc)]    = addCase3 dataName typeTransf stTran dtTran peTran pDtTran condSuc
addCaseList dataName typeTransf ((stTran, dtTran, peTran, pDtTran, condSuc):xs) = addCase3 dataName typeTransf stTran dtTran peTran pDtTran condSuc . addCaseList dataName typeTransf xs
  
  
-- M2 = this module contains the data types SPLModel and InstanceModel and TransformationModel of the Hephaestus's instance
selectAssetM2 :: String -> HsModule -> HsModule
selectAssetM2 n
  = addField "InstanceModel" sel
  . addField "SPLModel" sel'
  . addImportDecl ("HplAssets." ++ modtype)
  . addConstructor "TransformationModel" xtype "UndefinedTransformation"
  . addCaseList "xml2Transformation" xtype lstTransf
  where
  metadata = maybe (error ("Missing metadata for " ++ n ++ ".")) id $ lookup n assetMetaData
  modtype  = assetModuleType metadata
  sel'     = assetSelector' metadata
  sel      = assetSelector metadata
  xtype    = assetXType metadata
  lstTransf = assetLstTransf metadata
  
  
-- M1 = main module of the Hephaestus's instance. For exemple, Test.hs, Hephaestus.hs 
selectExportM1 :: String -> HsModule -> HsModule
selectExportM1 n 
  = addCase2 "export" xfun xtype xext sel
  . addImportDecl ("HplAssets." ++ mod)
 where
  metadata = maybe (error ("Missing metadata for " ++ n ++ ".")) id $ lookup n exportMetaData
  mod = exportModule metadata
  xtype = exportXType metadata
  xfun  = exportXFun metadata
  xext  = exportXExt metadata
  sel  = exportSelector metadata
  
  
-- M2 = this module contains the data types ExportModel and List lstExport of the Hephaestus's instance 
selectExportM2 :: String -> HsModule -> HsModule
selectExportM2 n 
  = addConstructorWithoutArgs "ExportModel" xtype "UndefinedExport"
  . addListElem "lstExport" xtype 
 where
  metadata = maybe (error ("Missing metadata for " ++ n ++ ".")) id $ lookup n exportMetaData
  mod = exportModule metadata
  xtype = exportXType metadata
  

-- insert CK parser instructions into Hephaestus's instance M1 module
selectCKParser::HsModule -> HsModule
selectCKParser = addImportDecl ("CK.Parsers.XML.XmlConfigurationParser")
               . addLetInstruction "main" "iModel" "findPropertyValue"  "cModel"  "configuration-model"  
               . addGeneratorInstruction "main" "parseFeatureModel" "cm" "parseConfigurationKnowledge" "(ns ckSchema) (snd cModel)" 
               . addLetInstruction' "main" "product" 
      

-- remove the "main" function of the Hephaestus's instance M1 module       
removeProductMainFunction :: HsModule -> HsModule
removeProductMainFunction = removeFunction "main" " "
                          . removeFunction "main" "funcBody"               
 
-- ----------------------------------------------------------
-- Hephaestus.hs
-- ----------------------------------------------------------
