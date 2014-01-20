module HplProducts.Hephaestus where
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
import HplAssets.Hephaestus.Parser.HephaestusParser
import HplAssets.Hephaestus.Types
import HplAssets.Hephaestus
 
data SPLModel = SPLModel{featureModel :: FeatureModel,
                         splHpl :: HephaestusModel}
 
data InstanceModel = InstanceModel{featureConfiguration ::
                                   FeatureConfiguration,
                                   hpl :: HephaestusModel}
                   deriving (Data, Typeable)
 
data TransformationModel = HephaestusTransformation HephaestusTransformation
 
data ExportModel = UndefinedExport
 
lstExport :: [ExportModel]
lstExport = []
 
xml2Transformation ::
                     String -> [String] -> ParserResult TransformationModel
xml2Transformation "selectBaseProduct" _
  = Success (HephaestusTransformation (SelectBaseProduct  ))
xml2Transformation "selectAsset" [id]
  = Success (HephaestusTransformation (SelectAsset id))
xml2Transformation "selectExport" [id]
  = Success (HephaestusTransformation (SelectExport id))
xml2Transformation "bindProductName" [id]
  = Success (HephaestusTransformation (BindProductName id))
xml2Transformation "removeProductMainFunction" _
  = Success (HephaestusTransformation (RemoveProductMainFunction  ))
xml2Transformation "selectCKParser" _
  = Success (HephaestusTransformation (SelectCKParser  ))
 
instance Transformation TransformationModel SPLModel InstanceModel
         where
        applyT t spl im = transform t spl (features im) im
 
transform ::
            TransformationModel ->
              SPLModel -> FeatureConfiguration -> InstanceModel -> InstanceModel
transform (HephaestusTransformation x0) x1 x2 x3
  = x3{hpl = transformHpl x0 (splHpl x1) (id x2) (hpl x3)}
 
instance SPL SPLModel InstanceModel where
        makeEmptyInstance fc spl = mkEmptyInstance fc spl
 
instance Product InstanceModel where
        features im = featureConfiguration im
 
mkEmptyInstance ::
                  FeatureConfiguration -> SPLModel -> InstanceModel
mkEmptyInstance fc spl
  = InstanceModel{featureConfiguration = fc,
                  hpl = emptyHpl (splHpl spl)}
 
export :: ExportModel -> FilePath -> InstanceModel -> IO ()
export (UndefinedExport) _ _ = undefined
 
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