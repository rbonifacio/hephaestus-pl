module HplProducts.HephaestusTypes where
import FeatureModel.Types hiding (Success, Fail)
import Data.Generics
import BasicTypes
import HplAssets.Hephaestus.Types
 
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
 
type ConfigurationKnowledge = [ConfigurationItem]
 
data ConfigurationItem = ConfigurationItem{expression ::
                                           FeatureExpression,
                                           transformations :: [TransformationModel]}
                       | ConstrainedConfigurationItem{expression :: FeatureExpression,
                                                      transformations :: [TransformationModel],
                                                      required :: FeatureExpression,
                                                      provided :: FeatureExpression}
 
constrained :: ConfigurationItem -> Bool
constrained (ConstrainedConfigurationItem _ _ _ _) = True
constrained _ = False
 
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