-- 
-- This is a command line module 
-- of Hephaestus, target to manage 
-- variability in use case models.
-- 
module Main where 

import System.IO
import System.Environment -- useful for getting program args
import System.Directory
import System.FilePath


import Maybe

import qualified BasicTypes as Core

import UseCaseModel.PrettyPrinter.Latex
import UseCaseModel.Parsers.XML.XmlUseCaseParser (parseUseCaseFile)
import UseCaseModel.Types

import ConfigurationKnowledge.Types
import ConfigurationKnowledge.Interpreter

import FeatureModel.Types 
import FeatureModel.Parsers.GenericParser 

import Transformations.Parsers.XML.XmlConfigurationParser

import ExportProduct (exportUcmToLatex)

type PropertyValue = (String, String)

-- some references to the rng schemas

fmSchema :: String 
fmSchema = "schema_feature-model.rng"

fcSchema :: String
fcSchema = "schema_feature-configuration.rng"

ckSchema :: String 
ckSchema = "schema-configuration-knowledge.rng"

ucSchema :: String
ucSchema = "schema_aspectual-use_cases-user_view.rng" 

normalizedSchema cDir sch = cDir </> sch 

main = do 
 cDir <- getCurrentDirectory
 let ns = normalizedSchema cDir
 
 print "========================================================="
 print "                                                         "       
 print " |__| _ ._ |_  _. _  __-+-. . __                         " 
 print " |  |(/,[_)[ )(_](/,_)  | (_|_)                          " 
 print "        |                                                "
 print "========================================================="
 print "                                                         " 
 print " Please, inform the name of the project file.            "
 print " It must specify, as pairs key=value, the path to the    "
 print " input files, such as:                                   "
 print "  name=projectName                                       "
 print "  feature-model={abs-path}/featureModel.xml              " 
 print "  configuration-model={abs-path}/configurationModel.xml  "
 print "  instance-model={abs-path}/instanceModel.xml            "
 print "  usecase-model={abs-path}/useCaseModel.xml              "
 print "  target-dir={abs-path}/                                 " 
 print "========================================================="
 print " ProjectFile:                                            " 
 
 f <- getLine	             -- read the name of the project file 
 s <- readFile f             -- read the file contents
 let l = lines s             -- split the content in several lines

 -- read all properties 
 let ps  = map fromJust (filter (isJust) (map readPropertyValue l))
 
 -- retrieve the specific property values we are interested in
 let n = fromJust (findPropertyValue "name" ps)
 let f = fromJust (findPropertyValue "feature-model" ps)
 let i = fromJust (findPropertyValue "instance-model" ps) 
 let c = fromJust (findPropertyValue "configuration-model" ps)
 let u = fromJust (findPropertyValue "usecase-model" ps)
 let t = fromJust (findPropertyValue "target-dir" ps)
          
 fmp <- parseFeatureModel  ((ns fmSchema), snd f) FMPlugin
 imp <- parseInstanceModel (ns fcSchema) (snd i)  
 cmp <- parseConfigurationKnowledge (ns ckSchema) (snd c)
 ucp <- parseUseCaseFile (ns ucSchema) (snd u)   
	  
 let pResults = (fmp, imp, cmp, ucp)
 case pResults of 
    ((Core.Success fm), (Core.Success im), (Core.Success cm), (Core.Success ucpl)) -> 
        do 
	 print " Great! We could say that you have done a good job. "
	 print " All files are syntactically correct.               "
	 print " Should we build your product [y/n]?                " 
	 
	 proceed <- getChar 
	 
         if proceed == 'y' 
          then do
	   let fc = createFC im
 	   let spl = createSPL fm ucpl 
           let product = build fm fc cm spl
           let out = (outputFile (snd t) (snd n))
	   let ucp = ucm product
           exportUcmToLatex out ucp
	   print $ "Ok, the output file was genarated at: " ++ out
          else print "Ok, closing your session. To start again, call the main function."

    ((Core.Fail e), _, _, _) -> print $ "Error parsing the feature model " ++ e 
    (_, (Core.Fail e), _, _) -> print $ "Error parsing the instance model " ++ e 
    (_, _, (Core.Fail e), _) -> print $ "Error parsing the configuration model " ++ e
    (_, _, _, (Core.Fail e)) -> print $ "Error parsing the use case model " ++ e 

 print "Done!" 
 
  

createSPL :: FeatureModel -> UseCaseModel -> SPLModel
createSPL fm ucm = SPLModel { splFM  = fm, splUCM = ucm }

outputFile :: FilePath -> String -> FilePath
outputFile  f n = f </> (n ++ ".tex") 

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

createFC im = FeatureConfiguration im