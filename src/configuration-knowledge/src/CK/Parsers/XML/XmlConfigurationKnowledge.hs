module CK.Parsers.XML.XmlConfigurationKnowledge where

import BasicTypes as Core 
import FeatureModel.Parsers.Expression
import FeatureModel.Types hiding (Success,Fail)
import Text.XML.HXT.Core
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language( haskellStyle )
import Data.List

import CK.Types         

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

xml2ConfigurationKnowledge :: (String -> [String] -> ParserResult a) -> XmlConfigurationKnowledge -> ParserResult (ConfigurationKnowledge a)
xml2ConfigurationKnowledge f ck
  = let cs = xmlConfigurations ck
        mcs = map (xml2Configuration f) cs
      in
      if and [isSuccess c | c <- mcs] then
        Success [ci | (Success ci) <- mcs] else
        Fail (unwords [showError e | e <- mcs, isSuccess e == False])
 
-- xml2Configuration  :: (String -> [String] -> (ParserResult a)) -> XmlConfiguration -> ParserResult (ConfigurationItem a)
xml2Configuration xml2Transformation c
  = let pe = parse parseExpression "" (xmlExpression c)
        ts = [xml2Transformation (tName t) (splitAndTrim ',' (tArgs t)) | t <- (xmlTransformations c)]
        pr = parseConstraint (xmlRequired c)
        pp = parseConstraint (xmlProvided c)
        pl = [pe, pr, pp]
      in
      if and [isSuccess t | t <- ts] then
        let pts = [a | (Success a) <- ts] in
          case pl of
              [Right exp, Right req, Right prov]  -> Success (ConstrainedConfigurationItem exp pts req prov)
              [Right exp, _, _]  -> Success (ConfigurationItem exp pts)
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

parseConstraint :: Maybe String -> Either ParseError FeatureExpression
parseConstraint (Nothing) = parse pzero "" ""
parseConstraint (Just s) = parse parseExpression "" s
 
