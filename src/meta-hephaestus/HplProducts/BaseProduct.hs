-- 
-- We include instructions for customizing the base product.
--

-- Set the module name to the product's main module
module HplProducts.BaseProduct where

-- Add imports for product-specific modules
-- Add import for CK.Parsers.XML.XmlConfigurationParser if asset is not Hephaestus
import FeatureModel.Types
import FeatureModel.Parsers.GenericParser 
import qualified BasicTypes as Core
import System.Directory
import System.FilePath
import Maybe 

import HplProducts.BaseProductTypes

-- Add equations for product-specific transformations
transform :: TransformationModel -> SPLModel -> InstanceModel -> InstanceModel
transform UndefinedTransformation _ _ = undefined

mkEmptyInstance :: FeatureConfiguration -> SPLModel -> InstanceModel
mkEmptyInstance fc spl =
  InstanceModel {
       featureConfiguration = fc
       -- Add product-specific model parts
  }
  
  
build :: FeatureModel                 -- ^ SPL feature model    
      -> FeatureConfiguration         -- ^ selection of features, which characterizes t
      -> ConfigurationKnowledge       -- ^ relationships between features and transformations
      -> SPLModel                     -- ^ SPL assets
      -> InstanceModel                -- ^ resulting instance of the build process
build fm fc ck spl = stepRefinement ts spl emptyInstance       
 where 
  emptyInstance = mkEmptyInstance fc spl
  ts = tasks ck fc      
  
        
tasks :: ConfigurationKnowledge -> FeatureConfiguration -> [TransformationModel]
tasks ck fc = concat [transformations c | c <- ck, eval fc (expression c)]
stepRefinement :: [TransformationModel] -> SPLModel -> InstanceModel -> InstanceModel
stepRefinement [] splModel instanceModel = instanceModel
stepRefinement (t:ts) splModel instanceModel
 = stepRefinement ts splModel (transform t splModel instanceModel)


-- Add equations for product-specific export
export :: ExportModel -> FilePath -> InstanceModel -> IO()
export UndefinedExport _ _ = undefined


-- the function main() to produce Hephaestus-PL instance
main :: IO()
main = do 
 cDir <- getCurrentDirectory
 let ns = normalizedSchema cDir
     
 f <- getLine	             -- read the name of the project file 
 s <- readFile f             -- read the file contents
 let l = lines s             -- split the content in several lines

 -- read all properties 
 let ps  = map fromJust (filter (isJust) (map readPropertyValue l))
 
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
type PropertyValue = (String, String)

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

-- given a String s, it returns just a property, 
-- if s matches "key=value". Otherwise, it returns 
-- Nothing.
readPropertyValue :: String -> Maybe PropertyValue
readPropertyValue s =
 let p = break (== '=') s
 in case p of 
     ([], _) -> Nothing
     (k , v) -> Just (k, tail v)  

findPropertyValue:: String -> [PropertyValue] -> Maybe PropertyValue  
findPropertyValue k [] = Nothing
findPropertyValue k (x:xs) =   
 if (k == fst x) then Just x
 else findPropertyValue k xs    
 
