-----------------------------------------------------------------------------
-- |
-- Module      :  UseCaseModel.Parsers.XML.XmlUseCaseModel
-- Copyright   :  (c) Rodrigo Bonifacio 2008, 2009
-- License     :  LGPL
--
-- Maintainer  :  rba2@cin.ufpe.br
-- Stability   :  provisional
-- Portability :  portable
--
-- Several functions for transforming a use case model to a TaRGeT XML 
-- representation. This module also define a representation in Haskell 
-- for the TaRGeT XML document representation
--
-----------------------------------------------------------------------------
module UseCaseModel.Parsers.XML.XmlUseCaseModel 

where

import List 

import UseCaseModel.Types
import BasicTypes

type XmlAction = String
type XmlState = String
type XmlResponse = String
type XmlFromStep = String
type XmlToStep = String
type XmlSetup = String

-- | The root element of the TaRGeT xml document.
--   Notice that this does not make scense, since 
--   TaRGeT is being used for specifying use cases 
--   in different domains. 

data XmlPhone = XmlPhone {
     ucms :: [XmlUseCaseModel]
} deriving (Show)

-- | The "feature" element of the TaRGeT xml document.
data XmlUseCaseModel = XmlUCM Id Name [XmlUseCase] [XmlAspectualUseCase]
	 deriving (Show)

-- | The use case TaRGeT element
data XmlUseCase = XmlUseCase Id Name Description XmlSetup [XmlScenario]
     deriving (Show)

-- | This is a new element for dealing with 
--   variabilities. Earlier versions of the TaRGeT XML documents 
--   do not have this element.      
data XmlAspectualUseCase = XmlAspectualUseCase { 
	xmlAspectId :: Id, 
 	xmlAspectName :: Name,
 	xmlAdvices :: [XmlAdvice] 
 }     
 deriving (Show)
     
-- | The scenlario TaRGeT element.
data XmlScenario = XmlScenario Id Description XmlFromStep XmlToStep [XmlStep]
	 deriving (Show)

-- | Another new element fot the TaRGeT XML documents.
--   It represents an advice, being a sublement of the 
--   XmlAspectualUseCase.
data XmlAdvice = XmlAdvice {
        xmlAdviceId :: String,
	xmlAdviceType :: String,
        xmlAdviceDescription :: String, 
	xmlPointcut :: String,
	xmlAdviceSteps :: [XmlStep]	
 }
 deriving (Show) 

-- | Another new element for the TaRGeT XML documents.
--   It represents an advice flow, being a subelement of 
--   XmlAdvice.
-- data XmlAdviceFlow = XmlAdviceFlow { 
--         xmlAdviceSteps ::  [XmlStep] 
--  } deriving (Show)

-- | The step TaRGeT element.
data XmlStep = XmlStep Id XmlAction XmlState XmlResponse 	 
	 deriving (Show)
	 
-- | Translate a Phone Document into a list of 
--   use case models. 
xmlPhone2UseCaseModels :: XmlPhone -> [UseCaseModel]
xmlPhone2UseCaseModels (XmlPhone xmlUCMs) = map xmlUseCaseModel2UseCaseModel xmlUCMs

-- | Translate a TaRGeT use case model to a base use case 
--   model. Note that this is a straightforward mapping. 
xmlUseCaseModel2UseCaseModel :: XmlUseCaseModel -> UseCaseModel
xmlUseCaseModel2UseCaseModel (XmlUCM umid name xmlUseCases xmlAspects) = 
	UCM name 
	    [xmlUseCase2UseCase xmlUseCase | xmlUseCase <- xmlUseCases] 
	    [xmlAspectualUseCase2AspectualUseCase xmlAspect | xmlAspect <- xmlAspects]

-- | Translate a TaRGeT use case to a base use case.
xmlUseCase2UseCase :: XmlUseCase -> UseCase
xmlUseCase2UseCase (XmlUseCase i n d s xmlScenarios) = 
 UseCase  i n  d [(xmlScenario2Scenario xmlScenario) | xmlScenario <- xmlScenarios] 

-- | Translate a TaRGeT aspectual use case to a base aspectual use case.
xmlAspectualUseCase2AspectualUseCase :: XmlAspectualUseCase -> AspectualUseCase
xmlAspectualUseCase2AspectualUseCase xmlAspect = 
 AspectualUseCase {
 	aspectId = (xmlAspectId xmlAspect),
 	aspectName = (xmlAspectName xmlAspect),
 	advices = [xmlAdvice2Advice xmlAdvice | xmlAdvice <- (xmlAdvices xmlAspect)]
 }

-- | Translate a TaRGeT advice to a base advice.
xmlAdvice2Advice :: XmlAdvice -> Advice
xmlAdvice2Advice (XmlAdvice i t d pc as) = 
 let 
  flow = [xmlStep2Step s | s <- as]
  refs = xmlStepRefs2StepRefs pc
  c = case t of 
 	"before" -> Before
 	"after" -> After
        "around" -> Around
 in (Advice c i d refs flow)		
  
-- | Translate a TaRGeT scenario to a scenario
xmlScenario2Scenario :: XmlScenario -> Scenario
xmlScenario2Scenario (XmlScenario sid description fromSteps toSteps steps) = scenario 
 where 
  scenario = Scenario sid 
 		     description 
 		     (xmlStepRefs2StepRefs fromSteps) 
 		     [xmlStep2Step step | step <- steps] 
 		     (xmlStepRefs2StepRefs toSteps)
 
-- | Translate a TaRGeT step to a base step
xmlStep2Step :: XmlStep -> Step
xmlStep2Step (XmlStep i a s r) = 
 let 
  ann = [tail ar | ar <- words (a ++ s ++ r), head ar == '@']
 in 
  if i == "PROCEED" then Proceed else Step i a s r ann
  
-- | Translate a TaRGeT step refs to a list of StepRef
--   A step ref might be either an IdRef or a AnnotationRef,
--   which must start with the '@' character. 
--
--   The start symbol of an annotation could be a variation
--   point. 
xmlStepRefs2StepRefs :: String -> [StepRef]
xmlStepRefs2StepRefs s = map xmlStepRefs2StepRefs' refs
 where  
  refs = [x | x <- (splitAndRemoveBlanks ',' s)]   
  xmlStepRefs2StepRefs' ref = 
   case ref of 
    ('@':ss) -> AnnotationRef ss
    otherwise -> IdRef ref  

            

     
