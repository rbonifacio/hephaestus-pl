module CK.Parsers.XML.XmlConfigurationKnowledge where

import BasicTypes
import FeatureModel.Parsers.Expression
import FeatureModel.Types hiding (Success,Fail)
import Text.XML.HXT.Core
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language( haskellStyle )
import Data.List
         
data XmlConfigurationKnowledge = XmlConfigurationKnowledge {
      xmlConfigurations :: [XmlConfiguration]
} deriving(Show)  
	
data XmlConfiguration = XmlConfiguration {
      xmlExpression:: String,
      xmlTransformations :: [XmlTransformation],
      xmlRequired :: Maybe String, 
      xmlProvided :: Maybe String
} deriving(Show)  	
	
data XmlTransformation = XmlTransformation {
      tName :: String,
      tArgs :: String
} deriving (Show)
