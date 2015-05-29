-- 
-- We include instructions for customizing the base product.
--

-- Set the module name to the product's main module
module HplProducts.Base where

-- Add imports for product-specific assets
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

data SPLModel = SPLModel {
  featureModel :: FeatureModel
  -- Add product-specific model parts
}

data InstanceModel = InstanceModel {
  featureConfiguration :: FeatureConfiguration
  -- Add product-specific model parts
} deriving (Data, Typeable)

-- Add embedding constructors for product-specific transformations
data TransformationModel = UndefinedTransformation

-- Add embedding constructors for product-specific exports. Used by function export of Base.hs
data ExportModel = UndefinedExport

-- Add embedding constructors for product-specific exports. Used by command sequence_ in function main() of Base.hs
lstExport::[ExportModel]
lstExport = []


-- Add equations for product-specific transformations
xml2Transformation :: String -> [String] -> ParserResult TransformationModel
xml2Transformation "Undefined" _ = undefined

-- Add equations for product-specific transformations

instance Transformation TransformationModel SPLModel InstanceModel where
  applyT t spl im = transform t spl (features im) im

transform :: TransformationModel -> SPLModel -> FeatureConfiguration -> InstanceModel -> InstanceModel
transform UndefinedTransformation _ _ _ = undefined

instance SPL SPLModel InstanceModel where 
  makeEmptyInstance fc spl = mkEmptyInstance fc spl
  
instance Product InstanceModel where  
  features im = featureConfiguration im
  
mkEmptyInstance :: FeatureConfiguration -> SPLModel -> InstanceModel
mkEmptyInstance fc spl =
  InstanceModel {
       featureConfiguration = fc
       -- Add product-specific model parts
  }
  

-- Add equations for product-specific export
export :: ExportModel -> FilePath -> InstanceModel -> IO()
export UndefinedExport _ _ = undefined

readProperties ps = ( fromJust (findPropertyValue "name" ps)
                    , fromJust (findPropertyValue "feature-model" ps)
                    , fromJust (findPropertyValue "instance-model" ps) )

main :: IO()
main = do 
 ns <- fmap normalizedSchema getCurrentDirectory 
-- ls <- fmap (lines . readFile) getLine 
 input <- getLine
 contents <- readFile input
 let ls = lines contents 

 -- read all properties 
 let ps  = map fromJust (filter (isJust) (map readPropertyValue ls))
 
 -- retrieve the specific property values we are interested in
 let name = fromJust (findPropertyValue "name" ps)
 let fModel = fromJust (findPropertyValue "feature-model" ps)
 let iModel = fromJust (findPropertyValue "instance-model" ps) 
 -- add command "let cModel = fromJust (findPropertyValue "configuration-model" ps)"
 let targetDir = fromJust (findPropertyValue "target-dir" ps)
 -- add command "findPropertyValue" to each asset of the product-specific 
          
 (Core.Success fm) <- parseFeatureModel  ((ns fmSchema), snd fModel) FMPlugin
 -- add command "(Core.Success cm) <- parseConfigurationKnowledge (ns ckSchema) (snd c)" if asset is not Hephaestus 
 (Core.Success im) <- parseInstanceModel (ns fcSchema) (snd iModel)  
 -- add command parser to each asset of the product-specific  
 
 let fc = FeatureConfiguration im
 --add in "spl" below the asset spl of the product-specific 
 let spl = SPLModel { featureModel = fm }
 --change "undefined" to "build fm fc cm spl"
 let product = undefined
     
 let out = (outputFile (snd targetDir) (snd name)) -- the function "export" will insert the extension of file's name n (.tex, .xml, etc)
 sequence_ [export x out product | x<-lstExport] 
 print $ "Ok, the output file was genarated at: " ++ out
  
 return()
  
-----------------------------------------------------------------------------------------
-- definitions brought from module Main.hs of Hephaestus 
-----------------------------------------------------------------------------------------       


fmSchema :: String 
fmSchema = "schema_feature-model.rng"

fcSchema :: String
fcSchema = "schema_feature-configuration.rng"

ckSchema :: String 
ckSchema = "schema-configuration-knowledge.rng"

normalizedSchema:: FilePath -> String -> FilePath
normalizedSchema cDir sch = cDir </> sch 

outputFile :: FilePath -> String -> FilePath
outputFile  f n = f </> n 

-- xml2ConfigurationKnowledge :: XmlConfigurationKnowledge -> ParserResult (ConfigurationKnowledge TransformationModel)
-- xml2ConfigurationKnowledge ck = 
--  let 
--    cs  = xmlConfigurations ck
--    mcs = map xml2Configuration cs
--  in 
--   if and [isSuccess c | c <- mcs]
--    then Success [ci | (Success ci) <- mcs]
--    else Fail (unwords [showError e | e <- mcs, isSuccess e == False])

-- xml2Configuration :: XmlConfiguration -> ParserResult (ConfigurationItem TransformationModel)
-- xml2Configuration c =
--  let 
--   pe = parse parseExpression "" (xmlExpression c)
--   ts = [xml2Transformation (tName t) (splitAndTrim ',' (tArgs t)) | t<-(xmlTransformations c)]
--   pr = parseConstraint (xmlRequired c)
--   pp = parseConstraint (xmlProvided c)  
--   pl = [pe, pr, pp] 
--  in 
--   if and [isSuccess t | t <-ts] then 
--    let pts = [a | (Success a) <- ts]
--    in 
--     case pl of
--       [Right exp, Right req, Right prov] -> Success (ConstrainedConfigurationItem { expression = exp
--      	    					                              , transformations = pts
-- 					        	 		      , required = req 
-- 					        			      , provided = prov })

--       [Right exp, _, _] -> Success (ConfigurationItem { expression = exp
--     	   	      	 	 		  , transformations = pts})     
  
--       otherwise -> Fail ("Error parsing configuration item with "    ++ 
--                          " expression " ++ (show $ xmlExpression c)  ++ 
--                          ", required "   ++ (show $ xmlRequired c)   ++ 
--                          ", provided "   ++ (show $ xmlProvided c)   ++ ". ")
--    else 
--     Fail (unwords [showError e | e <- ts, isSuccess e == False])   
--  where   
--   parseConstraint :: Maybe String -> Either ParseError FeatureExpression
--   parseConstraint Nothing = parse pzero "" ""
--   parseConstraint (Just s)  = parse parseExpression "" s

parseConfigurationKnowledge :: String -> String -> IO (ParserResult (ConfigurationKnowledge TransformationModel))
parseConfigurationKnowledge schema fileName
  = do
      result <- parseXmlConfigurationKnowledge schema fileName
      case result of
        (Core.Success xml) -> return $ xml2ConfigurationKnowledge xml2Transformation xml
        (Core.Fail err) -> return $ Core.Fail err
