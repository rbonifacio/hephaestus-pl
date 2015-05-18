module HplAssets.Hephaestus.MetaData where

import FeatureModel.Types
import HplAssets.Hephaestus.Types
import HplAssets.Hephaestus.MetaDataTypes
import Data.Map

--
-- An undefined feature model.
-- We do not need an actual feature model for composition.
-- (We only need configuration knowledge and feature configuration.)
--
noFeatureModel :: FeatureModel
noFeatureModel = undefined

--
-- Configuration knowledge for the Hephaestus PL.
--
configurationKnowledge :: [(FeatureExpression, [HephaestusTransformation])]
configurationKnowledge
 = [ (
       ConstantExpression True,
       [SelectBaseProduct]
     ),
     (
       FeatureRef "Hephaestus",
       [
         BindProductName "Hephaestus",
         SelectAsset "Hephaestus",
	 RemoveProductMainFunction
       ]
     ),      
     (
       Not (FeatureRef "Hephaestus"),
       [ SelectCKParser ]
     ),              
     (
       FeatureRef "UseCase",
       [SelectAsset "UseCase"]
     ),
     (
       And (FeatureRef "UseCase") (FeatureRef "UcmToXML"),
       [SelectExport "UcmToXML"]
     ),
     (
       And (FeatureRef "UseCase") (FeatureRef "UcmToLatex"),
       [SelectExport "UcmToLatex"]
     ),       
     (
       FeatureRef "BusinessProcess",
       [SelectAsset "BusinessProcess"]
     ),
     (
       And (FeatureRef "BusinessProcess") (FeatureRef "BpmToXML"),
       [SelectExport "BpmToXML"]
     ),  
     (
       FeatureRef "Requirement",
       [SelectAsset "Requirement"]
     ),
     (
       And (FeatureRef "Requirement") (FeatureRef "ReqToLatex"),
       [SelectExport "ReqToLatex"]
     ),         
     (
       FeatureRef "Code",
       [SelectAsset "Code", SelectExport "ExportSourceCode"]
     ), 
     (
       And (FeatureRef "Code") (FeatureRef "BuildFile"),
       [SelectExport "BuildFile"]
     ) 
   ]


--
-- Metadata about this assets used during composition.
--
assetMetaData = fromList
 [
  ( "UseCase",
    AssetMetaData {
       assetModuleType = "UCM.Types",
       assetModuleParser = "UCM.Parsers.XML.XmlUseCaseParser",
       assetModule = "UseCases",
       assetModel = "UseCaseModel",
       assetSelector = [("ucm","UseCaseModel")],
       assetSelector' = [("splUcm","UseCaseModel")],
       assetEmpty = "emptyUcm",
       assetXType = "UseCaseTransformation",
       assetXFun = "transformUcm",
       assetVarProperty = "uModel",
       assetNameProperty = "usecase-model",
       assetXFunParser = "parseUseCaseFile",
       assetVarParser = "ucpl",
       --assetParamParser = "(ns ucSchema) (snd " ++ assetVarProperty ++ ")",
       assetParamParser = "(ns ucSchema) (snd uModel)",
       -- Attention: the assetLstTransf list must be populated in the reverse order that the pattern matching should appear. For example:
       --"bindParameter _ " must be before than "bindParameter [x,y]".
       assetLstTransf = [("evaluateAspects", "EvaluateAspects", "ids", "ids", "Success"),
                         ("bindParameter", "BindParameter", "_", " ", "Fail"),
			 ("bindParameter", "BindParameter", "[x,y]", "x y", "Success"),
			 ("selectUseCases", "SelectUseCases", "ids", "ids", "Success"),
			 ("selectScenarios", "SelectScenarios", "ids", "ids", "Success") ] 
    }
  ),
  ( "BusinessProcess",
    AssetMetaData {
       assetModuleType = "BPM.Types",
       assetModuleParser = "BPM.Parsers.XML.XmlBusinessProcess",
       assetModule = "BusinessProcesses",
       assetModel = "BusinessProcessModel",
       assetSelector = [("bpm","BusinessProcessModel")],
       assetSelector' = [("splBpm","BusinessProcessModel")],
       assetEmpty = "emptyBpm",
       assetXType = "BusinessProcessTransformation",
       assetXFun = "transformBpm",
       assetVarProperty = "bModel",
       assetNameProperty = "businessprocess-model",
       assetXFunParser = "parseBusinessProcessFile",
       assetVarParser = "bppl",
       --assetParamParser = "(ns bpSchema) (snd " ++ assetVarProperty ++ ")",
       assetParamParser = "(ns bpSchema) (snd bModel)",
       assetLstTransf = [("bindParameterBpm", "BindParameterBpm", "_", " ", "Fail"),
			 ("bindParameterBpm", "BindParameterBpm", "[np,vp]", "np (Value vp)", "Success"),
			 ("evaluateAdvice", "EvaluateAdvice", "[id]", "id", "Success"),
			 ("selectBusinessProcess", "SelectBusinessProcess", "[id]", "id", "Success") ]  
    }
  ),
  ( "Requirement",
    AssetMetaData {
       assetModuleType = "ReqModel.Types",
       assetModuleParser = "ReqModel.Parsers.XML.XmlRequirementParser",
       assetModule = "Requirements",
       assetModel = "RequirementModel",
       assetSelector = [("req", "RequirementModel")],
       assetSelector' = [("splReq", "RequirementModel")],
       assetEmpty = "emptyReq",
       assetXType = "RequirementTransformation",
       assetXFun = "transformReq",
       assetVarProperty = "rModel",
       assetNameProperty = "requirement-model",
       assetXFunParser = "parseRequirementModel",
       assetVarParser = "reqpl",
       --assetParamParser = "(ns reqSchema) (snd " ++ assetVarProperty++")",
       assetParamParser = "(ns reqSchema) (snd rModel)",
       assetLstTransf = [("removeRequirements", "RemoveRequirements", "ids", "ids", "Success"),
			 ("selectRequirements", "SelectRequirements", "ids", "ids", "Success"),
			 ("selectAllRequirements", "SelectAllRequirements", "_", " ", "Success") ]  
    }
  ),
  ( "Code",
    AssetMetaData {
       assetModuleType = "ComponentModel.Types",
       assetModuleParser = "ComponentModel.Parsers.ParserComponentModel",
       assetModule = "Components",
       assetModel = "ComponentModel",
       assetSelector = [("componentModel", "ComponentModel")],
       assetSelector' = [("splComponentModel", "ComponentModel")],
       assetEmpty = "emptyComponent", --this asset extends function mkEmptyInstance in the Test.hs module inserting empty lists [] [] []
       assetXType = "ComponentTransformation",
       assetXFun = "transformComponent",
       assetVarProperty = "compModel",
       assetNameProperty = "component-model",
       assetXFunParser = "parseComponentModel",
       assetVarParser = "comppl",
       --assetParamParser = "(snd " ++ assetVarProperty++")",
       assetParamParser = "(snd compModel)",	 
       assetLstTransf = [("preprocessFiles", "PreProcessor", "e", "e", "Success"),
                         ("createBuildEntries", "CreateBuildEntries", "e", "e", "Success"),
                         ("selectAndMoveComponent", "SelectAndMoveComponent", "_", " ", "Fail"),
                         ("selectAndMoveComponent", "SelectAndMoveComponent", "[x,y]", "x y", "Success"),       
                         ("selectComponents", "SelectComponents", "ids", "ids", "Success")]  
    }
  ),
  ( "Hephaestus",
    AssetMetaData {
       assetModuleType = "Hephaestus.Types",
       assetModuleParser = "Hephaestus.Parser.HephaestusParser",
       assetModule = "Hephaestus",
       assetModel = "HephaestusModel",
       assetSelector = [("hpl", "HephaestusModel")], 
       assetSelector' = [("splHpl", "HephaestusModel")],
       assetEmpty = "emptyHpl",
       assetXType = "HephaestusTransformation",
       assetXFun = "transformHpl",
       assetVarProperty = " ",
       assetNameProperty = " ",
       assetXFunParser = " ",
       assetVarParser = " ",
       assetParamParser = " ",
       assetLstTransf = [ ("selectCKParser", "SelectCKParser", "_", " ", "Success"),
                          ("removeProductMainFunction", "RemoveProductMainFunction", "_", " ", "Success"),
                          ("bindProductName", "BindProductName", "[id]", "id", "Success"),
                          ("selectExport", "SelectExport", "[id]", "id", "Success"),
			  ("selectAsset", "SelectAsset", "[id]", "id", "Success"),
                          ("selectBaseProduct", "SelectBaseProduct", "_", " ", "Success") ]
    }
  )
 ]

--
-- Metadata about this asset's exportation used during composition.
--
exportMetaData = fromList
 [
  ( "UcmToXML",
    ExportMetaData {
       exportModule = "UCM.PrettyPrinter.XML",
       exportXType = "ExportUcmXML",
       exportXFun = "exportUcmToXML",
       exportXExt = ".xml",
       exportSelector = "ucm"
    }
  ),
  ( "UcmToLatex",
    ExportMetaData {
       exportModule = "UCM.PrettyPrinter.Latex",
       exportXType = "ExportUcmLatex",
       exportXFun = "exportUcmToLatex",
       exportXExt = ".tex",
       exportSelector = "ucm"
    }
  ),
  ( "BpmToXML",
    ExportMetaData {
       exportModule = "BPM.PrettyPrinter.XML",
       exportXType = "ExportBpmXML",
       exportXFun = "exportBpmToXML",
       exportXExt = ".xml",
       exportSelector = "bpm"
    }
  ),
  ( "ReqToLatex",
    ExportMetaData {
       exportModule = "ReqModel.PrettyPrinter.Latex",
       exportXType = "ExportReqLatex",
       exportXFun = "exportReqToLatex",
       exportXExt = ".tex",
       exportSelector = "req"
    }
  ),
 ( "ExportSourceCode", 
   ExportMetaData {
     exportModule = "ComponentModel.ExportSourceCode",
     exportXType = "ExportComponents",
     exportXFun = "exportSourceCode",
     exportXExt = "",
     exportSelector = "componentModel"   
   } 
 )
 ]
              