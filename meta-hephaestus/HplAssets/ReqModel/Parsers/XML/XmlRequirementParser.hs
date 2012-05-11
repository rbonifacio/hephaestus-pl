-----------------------------------------------------------------------------
-- |
-- Module      :  RequirementModel.Parsers.XmlRequirementParser
-- Copyright   :  (c) Rodrigo Bonifacio 2008, 2009
-- License     :  LGPL
--
-- Maintainer  :  rba2@cin.ufpe.br
-- Stability   :  provisional
-- Portability :  portable
--
-- A XML parser for our requirement model.
--
-----------------------------------------------------------------------------
module HplAssets.ReqModel.Parsers.XML.XmlRequirementParser (parseRequirementModel, reqSchema)
where 

import BasicTypes

import HplAssets.ReqModel.Types

import Text.XML.HXT.Core
import Text.XML.HXT.RelaxNG

import System.Environment

reqSchema :: String
reqSchema = "schema_requirements.rng"

-- | 
-- Parser for a requirement model file. 
-- It results either Success or Fail, if the file is 
-- not valid.
--
parseRequirementModel schema fileName  = 
 do
  errs <- checkRequirementFile schema fileName
  case errs of 
   [] -> do 
     reqModel <- parseRequirementModel' fileName
     return $ reqModel
   
   otherwise -> do
     let errs' = concat $ map show errs
     return $ Fail errs' 

checkRequirementFile schema fileName = 
 do 
   errs <- runX ( errorMsgCollect 
                  >>> 
                  readDocument [ withValidate yes
                               , withRelaxNG (createURI schema)
                               , withErrors yes                              
                               ] (createURI fileName)
                  >>>
                  getErrorMessages
                ) ;
   return errs

parseRequirementModel' fileName = 
 do
   c <- runX ( xunpickleDocument xpRequirementModel [ withValidate yes
 				                    , withTrace 1
 				                    , withRemoveWS yes
 				                    , withPreserveComment yes
                                                    ] (createURI fileName) )
   case c of 
     [x] -> return $ Success x
     otherwise -> return $ Fail "Error parsing the requirement model. Try to check the input file."


-- 
-- The parser implementation using HXT library.
-- It requires several picklers.
-- 
instance XmlPickler RequirementModel where
	xpickle = xpRequirementModel

instance XmlPickler Requirement where 
	xpickle = xpRequirement

xpRequirementModel :: PU RequirementModel
xpRequirementModel = 
	xpElem "requirementModel" $
	xpWrap ( RM, \ (RM r) -> (r) ) $
        (xpList xpRequirement)		 
			 
xpRequirement :: PU Requirement
xpRequirement = 	
	xpElem   "requirement" $
	xpWrap   ( uncurry3 Requirement, \ (Requirement i n d) -> (i, n, d) ) $
	xpTriple ( xpElem "id" xpText ) 
                 ( xpElem "name" xpText )
                 ( xpElem "description" xpText) 

