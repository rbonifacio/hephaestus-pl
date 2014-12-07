module HplProducts.Test where
import CK.Types
import CK.Parsers.XML.XmlConfigurationKnowledge
import CK.Parsers.XML.XmlConfigurationParser
import FeatureModel.Types hiding (Success, Fail)
import FeatureModel.Parsers.Expression
import FeatureModel.Parsers.GenericParser
import qualified BasicTypes as Core
import System.Directory
import System.FilePath
import Data.Maybe
import Data.Generics
import BasicTypes
import Text.ParserCombinators.Parsec
import HplAssets.UCM.Parsers.XML.XmlUseCaseParser
import HplAssets.UCM.Types
import HplAssets.UseCases
import HplAssets.UCM.PrettyPrinter.XML
import HplAssets.UCM.PrettyPrinter.Latex
import HplAssets.BPM.Parsers.XML.XmlBusinessProcess
import HplAssets.BPM.Types
import HplAssets.BusinessProcesses
import HplAssets.BPM.PrettyPrinter.XML
import HplAssets.ReqModel.Parsers.XML.XmlRequirementParser
import HplAssets.ReqModel.Types
import HplAssets.Requirements
import HplAssets.ReqModel.PrettyPrinter.Latex
import HplAssets.ComponentModel.Parsers.ParserComponentModel
import HplAssets.ComponentModel.Types
import HplAssets.Components
 
data SPLModel = SPLModel{featureModel :: FeatureModel,
                         splUcm :: UseCaseModel, splBpm :: BusinessProcessModel,
                         splReq :: RequirementModel, splMappings :: ComponentModel}
 
data InstanceModel = InstanceModel{featureConfiguration ::
                                   FeatureConfiguration,
                                   ucm :: UseCaseModel, bpm :: BusinessProcessModel,
                                   req :: RequirementModel, components :: [(Id, Id)],
                                   buildEntries :: [String], preProcessFiles :: [String]}
                   deriving (Data, Typeable)
 
data TransformationModel = UseCaseTransformation UseCaseTransformation
                         | BusinessProcessTransformation BusinessProcessTransformation
                         | RequirementTransformation RequirementTransformation
                         | ComponentTransformation ComponentTransformation
 
data ExportModel = ExportUcmXML
                 | ExportUcmLatex
                 | ExportBpmXML
                 | ExportReqLatex
 
lstExport :: [ExportModel]
lstExport
  = [ExportUcmXML, ExportUcmLatex, ExportBpmXML, ExportReqLatex]
 
xml2Transformation ::
                     String -> [String] -> ParserResult TransformationModel
xml2Transformation "selectScenarios" ids
  = Success (UseCaseTransformation (SelectScenarios ids))
xml2Transformation "selectUseCases" ids
  = Success (UseCaseTransformation (SelectUseCases ids))
xml2Transformation "bindParameter" [x,y]
  = Success (UseCaseTransformation (BindParameter x y))
xml2Transformation "bindParameter" _
  = Fail
      "Invalid number of arguments to the transformation bindParameter"
xml2Transformation "evaluateAspects" ids
  = Success (UseCaseTransformation (EvaluateAspects ids))
xml2Transformation "selectBusinessProcess" [id]
  = Success
      (BusinessProcessTransformation (SelectBusinessProcess id))
xml2Transformation "evaluateAdvice" [id]
  = Success (BusinessProcessTransformation (EvaluateAdvice id))
xml2Transformation "bindParameterBpm" [np,vp]
  = Success
      (BusinessProcessTransformation (BindParameterBpm np (Value vp)))
xml2Transformation "bindParameterBpm" _
  = Fail
      "Invalid number of arguments to the transformation bindParameterBpm"
xml2Transformation "selectAllRequirements" _
  = Success (RequirementTransformation (SelectAllRequirements  ))
xml2Transformation "selectRequirements" ids
  = Success (RequirementTransformation (SelectRequirements ids))
xml2Transformation "removeRequirements" ids
  = Success (RequirementTransformation (RemoveRequirements ids))
xml2Transformation "selectComponents" ids
  = Success (ComponentTransformation (SelectComponents ids))
xml2Transformation "selectAndMoveComponent" [x,y]
  = Success (ComponentTransformation (SelectAndMoveComponent x y))
xml2Transformation "selectAndMoveComponent" _
  = Fail
      "Invalid number of arguments to the transformation selectAndMoveComponent"
xml2Transformation "createBuildEntries" e
  = Success (ComponentTransformation (CreateBuildEntries e))
xml2Transformation "preprocessFiles" e
  = Success (ComponentTransformation (PreProcessor e))
 
instance Transformation TransformationModel SPLModel InstanceModel
         where
        applyT t spl im = transform t spl (features im) im
 
transform ::
            TransformationModel ->
              SPLModel -> FeatureConfiguration -> InstanceModel -> InstanceModel
transform (UseCaseTransformation x0) x1 x2 x3
  = x3{ucm = transformUcm x0 (splUcm x1) (id x2) (ucm x3)}
transform (BusinessProcessTransformation x0) x1 x2 x3
  = x3{bpm = transformBpm x0 (splBpm x1) (id x2) (bpm x3)}
transform (RequirementTransformation x0) x1 x2 x3
  = x3{req = transformReq x0 (splReq x1) (id x2) (req x3)}
transform (ComponentTransformation x0) x1 x2 x3
  = x3{preProcessFiles =
         transformComponent x0 (splMappings x1) (id x2)
           (preProcessFiles x3)}
 
instance SPL SPLModel InstanceModel where
        makeEmptyInstance fc spl = mkEmptyInstance fc spl
 
instance Product InstanceModel where
        features im = featureConfiguration im
 
mkEmptyInstance ::
                  FeatureConfiguration -> SPLModel -> InstanceModel
mkEmptyInstance fc spl
  = InstanceModel{featureConfiguration = fc,
                  ucm = emptyUcm (splUcm spl), bpm = emptyBpm (splBpm spl),
                  req = emptyReq (splReq spl), components = [], buildEntries = [],
                  preProcessFiles = []}
 
export :: ExportModel -> FilePath -> InstanceModel -> IO ()
export (ExportUcmXML) x1 x2
  = exportUcmToXML (x1 ++ ".xml") (ucm x2)
export (ExportUcmLatex) x1 x2
  = exportUcmToLatex (x1 ++ ".tex") (ucm x2)
export (ExportBpmXML) x1 x2
  = exportBpmToXML (x1 ++ ".xml") (bpm x2)
export (ExportReqLatex) x1 x2
  = exportReqToLatex (x1 ++ ".tex") (req x2)
readProperties ps
  = (fromJust (findPropertyValue "name" ps),
     fromJust (findPropertyValue "feature-model" ps),
     fromJust (findPropertyValue "instance-model" ps))
 
main :: IO ()
main
  = do ns <- fmap normalizedSchema getCurrentDirectory
       input <- getLine
       contents <- readFile input
       let ls = lines contents
       let ps = map fromJust (filter (isJust) (map readPropertyValue ls))
       let name = fromJust (findPropertyValue "name" ps)
       let fModel = fromJust (findPropertyValue "feature-model" ps)
       let iModel = fromJust (findPropertyValue "instance-model" ps)
       let cModel = fromJust (findPropertyValue "configuration-model" ps)
       let targetDir = fromJust (findPropertyValue "target-dir" ps)
       let compModel = fromJust (findPropertyValue "component-model" ps)
       let rModel = fromJust (findPropertyValue "requirement-model" ps)
       let bModel
             = fromJust (findPropertyValue "businessprocess-model" ps)
       let uModel = fromJust (findPropertyValue "usecase-model" ps)
       (Core.Success fm) <- parseFeatureModel ((ns fmSchema), snd fModel)
                              FMPlugin
       (Core.Success cm) <- parseConfigurationKnowledge
                              (ns ckSchema) (snd cModel)
       (Core.Success im) <- parseInstanceModel (ns fcSchema) (snd iModel)
       (Core.Success comppl) <- parseComponentModel (snd compModel)
       (Core.Success reqpl) <- parseRequirementModel
                                 (ns reqSchema) (snd rModel)
       (Core.Success bppl) <- parseBusinessProcessFile
                                (ns bpSchema) (snd bModel)
       (Core.Success ucpl) <- parseUseCaseFile (ns ucSchema) (snd uModel)
       let fc = FeatureConfiguration im
       let spl
             = SPLModel{featureModel = fm, splUcm = ucpl, splBpm = bppl,
                        splReq = reqpl, splMappings = comppl}
       let product = build fm fc cm spl
       let out = (outputFile (snd targetDir) (snd name))
       sequence_ [export x out product | x <- lstExport]
       print $ "Ok, the output file was genarated at: " ++ out
       return ()
 
fmSchema :: String
fmSchema = "schema_feature-model.rng"
 
fcSchema :: String
fcSchema = "schema_feature-configuration.rng"
 
ckSchema :: String
ckSchema = "schema-configuration-knowledge.rng"
 
normalizedSchema :: FilePath -> String -> FilePath
normalizedSchema cDir sch = cDir </> sch
 
outputFile :: FilePath -> String -> FilePath
outputFile f n = f </> n
 
xml2ConfigurationKnowledge ::
                             XmlConfigurationKnowledge ->
                               ParserResult (ConfigurationKnowledge TransformationModel)
xml2ConfigurationKnowledge ck
  = let cs = xmlConfigurations ck
        mcs = map xml2Configuration cs
      in
      if and [isSuccess c | c <- mcs] then
        Success [ci | (Success ci) <- mcs] else
        Fail (unwords [showError e | e <- mcs, isSuccess e == False])
 
xml2Configuration ::
                    XmlConfiguration ->
                      ParserResult (ConfigurationItem TransformationModel)
xml2Configuration c
  = let pe = parse parseExpression "" (xmlExpression c)
        ts
          = [xml2Transformation (tName t) (splitAndTrim ',' (tArgs t)) |
             t <- (xmlTransformations c)]
        pr = parseConstraint (xmlRequired c)
        pp = parseConstraint (xmlProvided c)
        pl = [pe, pr, pp]
      in
      if and [isSuccess t | t <- ts] then
        let pts = [a | (Success a) <- ts] in
          case pl of
              [Right exp, Right req, Right prov]
                -> Success
                     (ConstrainedConfigurationItem{expression = exp,
                                                   transformations = pts, required = req,
                                                   provided = prov})
              [Right exp, _, _]
                -> Success
                     (ConfigurationItem{expression = exp, transformations = pts})
              otherwise
                -> Fail
                     ("Error parsing configuration item with " ++ " expression " ++
                        (show $ xmlExpression c)
                        ++ ", required "
                        ++ (show $ xmlRequired c)
                        ++ ", provided "
                        ++ (show $ xmlProvided c)
                        ++ ". ")
        else Fail (unwords [showError e | e <- ts, isSuccess e == False])
  where  
        parseConstraint ::
                          Maybe String -> Either ParseError FeatureExpression
        parseConstraint (Nothing) = parse pzero "" ""
        parseConstraint (Just s) = parse parseExpression "" s
 
parseConfigurationKnowledge ::
                              String ->
                                String ->
                                  IO (ParserResult (ConfigurationKnowledge TransformationModel))
parseConfigurationKnowledge schema fileName
  = do result <- parseXmlConfigurationKnowledge schema fileName
       case result of
           (Core.Success xml) -> return $ xml2ConfigurationKnowledge xml
           (Core.Fail err) -> return $ Core.Fail err