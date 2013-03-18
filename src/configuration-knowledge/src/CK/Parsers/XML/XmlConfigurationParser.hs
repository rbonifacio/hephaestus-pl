-----------------------------------------------------------------------------
-- |
-- Module      :  CK.Parsers.XML.XmlConfigurationParser
-- License     :  LGPL
--
-- Stability   :  under construction
-- Portability :  unknown
--
-- An XML parser for the configuration knowledge.
-- This module does not yet use the asset-specific typed representation of CK.
--
-----------------------------------------------------------------------------
module CK.Parsers.XML.XmlConfigurationParser (parseXmlConfigurationKnowledge)

where

import BasicTypes 
import Text.XML.HXT.Core
import Text.XML.HXT.RelaxNG
import System.Environment
import CK.Parsers.XML.XmlConfigurationKnowledge

-- | 
-- XML parser for a configuration knowledge file. 
-- It results either Success or Fail, if the file is 
-- not valid.
--
parseXmlConfigurationKnowledge schema fileName = 
  do
  errs <- checkConfigurationFile schema fileName
  case errs of 
   [] -> do 
     configuration <- parseXmlConfigurationKnowledge' fileName
     return $ configuration
   
   otherwise -> do
     let errs' = concat $ map show errs
     return $ Fail errs' 

checkConfigurationFile schema fileName = 
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

parseXmlConfigurationKnowledge' fileName = 
 do
   c <- runX ( xunpickleDocument xpConfigurationKnowledge [ withValidate yes
 				                          , withTrace 1
 				                          , withRemoveWS yes
 				                          , withPreserveComment yes
                                                          ] (createURI fileName) )
   case c of 
     [x] -> return $ Success x
     otherwise -> return $ Fail "Unexpected error found when parsing the configuration knowledge."
   
-- 
-- The parser implementation using HXT library.
-- It requires several picklers.
-- 
instance XmlPickler XmlConfigurationKnowledge where
	xpickle = xpConfigurationKnowledge

instance XmlPickler XmlConfiguration where 
	xpickle = xpConfiguration
	
instance XmlPickler XmlTransformation where
 	xpickle = xpTransformation	

xpConfigurationKnowledge :: PU XmlConfigurationKnowledge
xpConfigurationKnowledge =
	xpElem "configurationModel" $
	xpWrap ( XmlConfigurationKnowledge, \ (XmlConfigurationKnowledge c) -> (c) ) $
        (xpList xpConfiguration)		 
			 
xpConfiguration :: PU XmlConfiguration
xpConfiguration = 	
	xpElem "configuration" $
	xpWrap ( uncurry4 XmlConfiguration, \ (XmlConfiguration e t r p) -> (e, t, r, p) ) $
	xp4Tuple ( xpElem "expression" xpText ) 
	         ( xpList xpTransformation ) 
                 ( xpOption ( xpElem "required" xpText ) ) 
                 ( xpOption ( xpElem "provided" xpText ) ) 
         
	
xpTransformation :: PU XmlTransformation
xpTransformation = 	
	xpElem "transformation" $
	xpWrap ( uncurry XmlTransformation, \ (XmlTransformation n a) -> (n, a) ) $
	xpPair ( xpElem "name" xpText ) ( xpElem "args" xpText )
