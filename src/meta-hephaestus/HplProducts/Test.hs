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
 
data SPLModel = SPLModel{featureModel :: FeatureModel,
                         splUcm :: UseCaseModel}
 
data InstanceModel = InstanceModel{featureConfiguration ::
                                   FeatureConfiguration,
                                   ucm :: UseCaseModel}
                   deriving (Data, Typeable)
 
data TransformationModel = UseCaseTransformation UseCaseTransformation
 
data ExportModel = ExportUcmXML
 
lstExport :: [ExportModel]
lstExport = [ExportUcmXML]
 
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
 
instance Transformation TransformationModel SPLModel InstanceModel
         where
        applyT t spl im = transform t spl (features im) im
 
transform ::
            TransformationModel ->
              SPLModel -> FeatureConfiguration -> InstanceModel -> InstanceModel
transform (UseCaseTransformation x0) x1 x2 x3
  = x3{ucm = transformUcm x0 (splUcm x1) (id x2) (ucm x3)}
 
instance SPL SPLModel InstanceModel where
        makeEmptyInstance fc spl = mkEmptyInstance fc spl
 
instance Product InstanceModel where
        features im = featureConfiguration im
 
mkEmptyInstance ::
                  FeatureConfiguration -> SPLModel -> InstanceModel
mkEmptyInstance fc spl
  = InstanceModel{featureConfiguration = fc,
                  ucm = emptyUcm (splUcm spl)}
 
export :: ExportModel -> FilePath -> InstanceModel -> IO ()
export (ExportUcmXML) x1 x2
  = exportUcmToXML (x1 ++ ".xml") (ucm x2)
 
main :: IO ()
main
  = do cDir <- getCurrentDirectory
       let ns = normalizedSchema cDir
       f <- getLine
       s <- readFile f
       let l = lines s
       let ps = map fromJust (filter (isJust) (map readPropertyValue l))
       let name = fromJust (findPropertyValue "name" ps)
       let fModel = fromJust (findPropertyValue "feature-model" ps)
       let iModel = fromJust (findPropertyValue "instance-model" ps)
       let cModel = fromJust (findPropertyValue "configuration-model" ps)
       let targetDir = fromJust (findPropertyValue "target-dir" ps)
       let uModel = fromJust (findPropertyValue "usecase-model" ps)
       (Core.Success fm) <- parseFeatureModel ((ns fmSchema), snd fModel)
                              FMPlugin
       (Core.Success cm) <- parseConfigurationKnowledge
                              (ns ckSchema) (snd cModel)
       (Core.Success im) <- parseInstanceModel (ns fcSchema) (snd iModel)
       (Core.Success ucpl) <- parseUseCaseFile (ns ucSchema) (snd uModel)
       let fc = FeatureConfiguration im
       let spl = SPLModel{featureModel = fm, splUcm = ucpl}
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