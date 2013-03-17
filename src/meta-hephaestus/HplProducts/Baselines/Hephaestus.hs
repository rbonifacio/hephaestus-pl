module HplProducts.Hephaestus where
import FeatureModel.Types
import FeatureModel.Parsers.GenericParser
import qualified BasicTypes as Core
import System.Directory
import System.FilePath
import Data.Maybe
import HplProducts.HephaestusTypes
import HplAssets.Hephaestus.Parser.HephaestusParser
import HplAssets.Hephaestus
 
transform ::
            TransformationModel ->
              SPLModel -> FeatureConfiguration -> InstanceModel -> InstanceModel
transform (HephaestusTransformation x0) x1 x2 x3
  = x3{hpl = transformHpl x0 (splHpl x1) (id x2) (hpl x3)}
 
mkEmptyInstance ::
                  FeatureConfiguration -> SPLModel -> InstanceModel
mkEmptyInstance fc spl
  = InstanceModel{featureConfiguration = fc,
                  hpl = emptyHpl (splHpl spl)}
 
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
  = stepRefinement ts splModel
      (transform t splModel (featureConfiguration instanceModel)
         instanceModel)
 
export :: ExportModel -> FilePath -> InstanceModel -> IO ()
export (UndefinedExport) _ _ = undefined
 
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