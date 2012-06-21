module HplProducts.TestTypes where
import FeatureModel.Types hiding (Success, Fail)
import Data.Generics
import BasicTypes
import HplAssets.ComponentModel.Types
 
data SPLModel = SPLModel{featureModel :: FeatureModel,
                         splMappings :: ComponentModel}
 
data InstanceModel = InstanceModel{featureConfiguration ::
                                   FeatureConfiguration,
                                   components :: [(Id, Id)], buildEntries :: [String],
                                   preProcessFiles :: [String]}
                   deriving (Data, Typeable)
 
data TransformationModel = ComponentTransformation ComponentTransformation
 
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