module HplProducts.Test where
import FeatureModel.Types
import FeatureModel.Parsers.GenericParser
import qualified BasicTypes as Core
import System.Directory
import System.FilePath
import Data.Maybe
import CK.Parsers.XML.XmlConfigurationParser
import HplProducts.TestTypes
import HplAssets.UCM.Parsers.XML.XmlUseCaseParser
import HplAssets.UseCases
import HplAssets.UCM.PrettyPrinter.XML
 
transform ::
            TransformationModel -> SPLModel -> InstanceModel -> InstanceModel
transform (UseCaseTransformation x0) x1 x2 = transformUcm  x0 x1 x2
 
mkEmptyInstance ::
                  FeatureConfiguration -> SPLModel -> InstanceModel
mkEmptyInstance fc spl
  = InstanceModel{featureConfiguration = fc,
                  ucm = emptyUcm (splUcm spl)}
 
build ::
        FeatureModel ->
          FeatureConfiguration ->
            ConfigurationKnowledge -> SPLModel -> InstanceModel
build fm fc ck spl = stepRefinement ts spl emptyInstance
  where emptyInstance = mkEmptyInstance fc spl
        ts = tasks ck fc
 
tasks ::
        ConfigurationKnowledge ->
          FeatureConfiguration -> [TransformationModel]
tasks ck fc
  = concat [transformations c | c <- ck, eval fc (expression c)]
 
stepRefinement ::
                 [TransformationModel] -> SPLModel -> InstanceModel -> InstanceModel
stepRefinement [] splModel instanceModel = instanceModel
stepRefinement (t : ts) splModel instanceModel
  = stepRefinement ts splModel (transform t splModel instanceModel)
 
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
 
type PropertyValue = (String, String)
 
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
 
readPropertyValue :: String -> Maybe PropertyValue
readPropertyValue s
  = let p = break (== '=') s in
      case p of
          ([], _) -> Nothing
          (k, v) -> Just (k, tail v)
 
findPropertyValue ::
                    String -> [PropertyValue] -> Maybe PropertyValue
findPropertyValue k [] = Nothing
findPropertyValue k (x : xs)
  = if (k == fst x) then Just x else findPropertyValue k xs