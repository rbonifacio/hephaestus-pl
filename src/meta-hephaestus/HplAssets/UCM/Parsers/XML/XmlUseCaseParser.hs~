-----------------------------------------------------------------------------
-- |
-- Module      :  UseCaseModel.Parsers.XML.XmlUseCaseParser
-- Copyright   :  (c) Rodrigo Bonifacio 2008, 2009
-- License     :  LGPL
--
-- Maintainer  :  rba2@cin.ufpe.br
-- Stability   :  provisional
-- Portability :  portable
--
-- This module defines several functions for parsing 
-- a TaRGeT xml document. We develped this module using the HXT (Haskell XML 
-- Toolkit) library. 
--
-----------------------------------------------------------------------------
module HplAssets.UCM.Parsers.XML.XmlUseCaseParser -- (parseUseCaseFile, checkUseCaseFile, xpUseCaseModel)
where

import BasicTypes
import HplAssets.UCM.Parsers.XML.XmlUseCaseModel

import Text.XML.HXT.Core
import Text.XML.HXT.RelaxNG


import System.Environment

-- | Parse a use case file, returning the corresponding use case model.
--   The second parameter must have a valid XML schema. 
--   If the input file is not well formed, a fail is reported. Otherwise, 
--   one instance of the use case model. 

ucSchema :: String
ucSchema = "schema_aspectual-use_cases-user_view.rng"

parseUseCaseFile schema fileName  = 
 do
  errs <- checkUseCaseFile schema fileName
  case errs of 
   [] -> do 
     ucModel <- parseUseCaseFile' fileName
     return $ Success ucModel
   
   otherwise -> do
     let errs' = concat $ map show errs
     return $ Fail errs' 

-- | Checks if a use case file is well formed, according 
--   to the XML schema passed as the second argument. This 
--   function returns the list of errors identified. An empty 
--   list will be returned, if no error is found. 
checkUseCaseFile schema fileName = 
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

-- Assuming the file 'fileName' is valid, we could proceed parsing it.
 
parseUseCaseFile' fileName = 
 do
   [x] <- runX ( xunpickleDocument xpPhone [ withValidate yes
 					   , withTrace 1
 					   , withRemoveWS yes
 					   , withPreserveComment yes
 					   ] (createURI fileName) )
   let ucmodel = xmlUseCaseModel2UseCaseModel (head (ucms x))
   return ucmodel
      
--
-- based on the HXT library, we have to declare one instance of 
-- XmlPickler for each element of our data structures. 
-- 
instance XmlPickler XmlPhone where 
         xpickle = xpPhone

instance XmlPickler XmlUseCaseModel where
	xpickle = xpUseCaseModel

instance XmlPickler XmlUseCase where
	xpickle = xpUseCase
	
instance XmlPickler XmlAspectualUseCase where 
	xpickle = xpAspectualUseCase	

instance XmlPickler XmlAdvice where 
	xpickle = xpAdvice
		
instance XmlPickler XmlScenario where 
 	xpickle = xpScenario

-- instance XmlPickler XmlAdviceFlow where 
--        xpickle = xpAdviceFlow 

instance XmlPickler XmlStep where 
	xpickle = xpStep
-- 
-- necessary, if the element has more than five sub-elements or attributes.	
--
uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a, b, c, d, e) -> f
uncurry5 fn (a, b, c, d, e) = fn a b c d e 

xpPhone :: PU XmlPhone
xpPhone = 
        xpElem "phone" $
        xpWrap (XmlPhone, \ (XmlPhone ucms) -> (ucms) ) $ 
        xpList (xpUseCaseModel)

xpUseCaseModel :: PU XmlUseCaseModel
xpUseCaseModel =
	xpElem "feature" $ -- it should be useCaseModel, but we change to TaRGeT format.
	xpWrap ( uncurry4 XmlUCM, \ (XmlUCM i n ucs aspect) -> (i, n, ucs, aspect) ) $
	xp4Tuple ( xpElem "featureId" xpText ) 
                 ( xpElem "name" xpText ) 
                 ( xpList xpUseCase )  
                 ( xpList xpAspectualUseCase )

xpUseCase :: PU XmlUseCase
xpUseCase =
	xpElem "useCase" $
	xpWrap ( uncurry5 XmlUseCase, \ (XmlUseCase i n d s ss) -> (i, n, d, s, ss) ) $
	xp5Tuple (xpElem "id" xpText) 
                 (xpElem "name" xpText) 
                 (xpElem "description" xpText) 
                 (xpElem "setup" xpText) 
                 (xpList xpScenario)

xpAspectualUseCase :: PU XmlAspectualUseCase
xpAspectualUseCase = 
	xpElem "aspect"	$
	xpWrap ( uncurry3 XmlAspectualUseCase, \ (XmlAspectualUseCase i n a) -> (i, n,a) ) $
	xpTriple (xpElem "id" xpText) (xpElem "name" xpText) (xpList xpAdvice)	

xpAdvice :: PU XmlAdvice
xpAdvice = 
	xpElem "advice" $
	xpWrap ( uncurry5 XmlAdvice, \ (XmlAdvice i t d p s) -> (i, t, d, p, s) ) $
	xp5Tuple (xpElem "id" xpText)
                 (xpElem "type" xpText)                 
                 (xpElem "description" xpText)
                 (xpElem "pointCut" xpText) 
                 (xpList xpStep)

xpScenario :: PU XmlScenario
xpScenario = 
	xpElem "flow" $
	xpWrap ( uncurry5 XmlScenario, \ (XmlScenario i d f t s) -> (i, d, f, t, s) ) $
	xp5Tuple (xpElem "id" xpText )
	         (xpElem "description" xpText ) 
		 (xpElem "fromSteps" xpText) 
		 (xpElem "toSteps" xpText) 
		 (xpList xpStep) 

-- xpAdviceFlow :: PU XmlAdviceFlow
-- xpAdviceFlow = 
--         xpElem "aspectualFlow" $
--         xpWrap (XmlAdviceFlow, \ (XmlAdviceFlow s) -> (s) ) $
--        (xpList xpStep) 

xpStep :: PU XmlStep 
xpStep = 
	xpElem "step" $
	xpWrap ( uncurry4 XmlStep, \ (XmlStep i a  s r) -> (i, a, s, r) ) $
	xp4Tuple (xpElem "stepId" xpText) 
                 (xpElem "action" xpText ) 
                 (xpElem "condition" xpText) 
                 (xpElem "response" xpText)

	

