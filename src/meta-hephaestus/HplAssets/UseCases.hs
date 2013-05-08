module HplAssets.UseCases (
  transformUcm,
  emptyUcm
) where

import BasicTypes
import HplAssets.UCM.Types
import FeatureModel.Types
import Data.Generics

emptyUcm :: UseCaseModel -> UseCaseModel
emptyUcm ucmodel = ucmodel { useCases = [], aspects = [] }

emptyUseCase :: UseCase -> UseCase
emptyUseCase uc = uc { ucScenarios = [] }

transformUcm :: UseCaseTransformation -> UseCaseModel -> FeatureConfiguration -> UseCaseModel -> UseCaseModel
transformUcm (SelectUseCases ids) spl _ product = 
  addScenariosToInstance (scs, spl, product) 
  where scs = concat $ map ucScenarios [uc | uc <- useCases spl, ucId uc `elem` ids]
        
transformUcm (SelectScenarios ids) spl _ product = 
  addScenariosToInstance (scs, spl, product) 
  where scs = [s | s <- ucmScenarios spl, scId s `elem` ids]
        
transformUcm (BindParameter pid fid) spl fc product = 
  bindParameter steps (parenthesize options) pid product 
  where 
    steps = [s | s <- ucmSteps product, s `refers` pid] 
    options = concat (map featureOptionsValues [f | f <- flatten (fcTree fc), fId (fnode f) == fid]) 
    bindParameter [] o pid p = p 
    bindParameter (s:ss) o pid p = bindParameter ss o pid (gReplaceParameterInScenario (sId s) pid o p)
    
transformUcm (EvaluateAspects ids) spl _ product = 
  evaluateListOfAdvice as product 
  where as = concat [advices a | a <- aspects spl, (aspectId a) `elem` ids]


-- this is a map function that adds a list of scenarios 
-- to a use case model.
addScenariosToInstance :: ([Scenario], UseCaseModel, UseCaseModel) -> UseCaseModel
addScenariosToInstance ([], spl, product) = product
addScenariosToInstance ((s:ss), spl, product) = addScenariosToInstance (ss, spl, product') 
 where 
  product' = addScenarioToInstance (s, sUseCase, product) 
  sUseCase = findUseCaseFromScenario (useCases spl) s

-- add a single scenario to a use case model.
addScenarioToInstance :: (Scenario, Maybe UseCase, UseCaseModel) -> UseCaseModel
addScenarioToInstance (s, Nothing, product) =  error "Scenario not declared within a use case"
addScenarioToInstance (s, (Just sUseCase), product)  = 
 let
  pUseCase = [u | u <- useCases product, (ucId sUseCase) == (ucId u)]
  eUseCase = (emptyUseCase sUseCase) { ucScenarios = [s] } 
 in case pUseCase of
     []  -> gAddUseCase eUseCase product
     [u] -> gAddScenario (ucId u) s product

-- this is the generic function for adding a scenario.
-- it follows the SYB pattern. 
gAddScenario :: Id -> Scenario -> UseCaseModel -> UseCaseModel
gAddScenario i s = everywhere (mkT (addOrReplaceScenario i s))

-- this is the generic function for adding a use case. 
-- it follows the SYB pattern. 
gAddUseCase :: UseCase -> UseCaseModel -> UseCaseModel 
gAddUseCase u = everywhere (mkT (addOrReplaceUseCase u))
      
-- add or replace a scenarion to a use case. this is 
-- an auxiliarly function to the 'selectScenario' transformation.
addOrReplaceScenario :: Id -> Scenario -> UseCase -> UseCase
addOrReplaceScenario i sc uc =
 if (ucId uc == i) 
  then 
   let scs = ucScenarios uc 
   in uc { ucScenarios = [s | s <- scs, s /= sc] ++ [sc]}
  else uc 

-- add or replace a use case to a use case model. this is 
-- an auxiliarly function to the 'selectScenario' transformation.
addOrReplaceUseCase :: UseCase -> UseCaseModel -> UseCaseModel
addOrReplaceUseCase uc ucModel = 
 let ucs = useCases ucModel 
 in ucModel { useCases = [u | u <- ucs, u /= uc ] ++ [uc]}    


-- evaluate a list of advices
evaluateListOfAdvice :: [Advice] -> UseCaseModel -> UseCaseModel
evaluateListOfAdvice [] p = p
evaluateListOfAdvice (x:xs) p = evaluateListOfAdvice xs (genEvaluateAdvice x p)

-- this is the generic function for evaluating 
-- an advice. It follows the Scrap Your Boilerplate (SYB)
-- pattern. 
genEvaluateAdvice :: Advice -> UseCaseModel -> UseCaseModel  
genEvaluateAdvice a = everywhere (mkT (evaluateAdvice a))

evaluateAdvice :: Advice -> Scenario -> Scenario
evaluateAdvice a s = foldl (compose a) s pcs 
 where pcs = pointCut a

compose :: Advice -> Scenario  -> StepRef -> Scenario
compose adv sc pc = 
 if (matches pc sc) 
  then sc { steps = (compose' (advType adv)) aFlow sFlow }
  else sc
 where 
  compose' (Before) = concatBefore (match pc)
  compose' (After)  = concatAfter  (match pc)
  compose' (Around) = concatAround (match pc) proceed
  aFlow = aspectualFlow adv
  sFlow = steps sc

-- just an auxiliarly function for checking if a 
-- step refers to a parameter. this function might 
-- be parameterized with different delimiters.   
refers :: Step -> Id -> Bool
refers s pid = 
 let 
  w = "{" ++ pid ++ "}" 
  wExists = existsWord w
 in (wExists (action s)) || (wExists (state s)) || (wExists (response s))
 
replaceParameterInScenario :: Id -> String -> String -> Scenario -> Scenario
replaceParameterInScenario sid fp ap scn = 
 let sts = steps scn 
 in scn { steps = map (replaceStringInStep sid fp ap) sts }

-- just an auxiliarly function for replacin a string in a step. 
-- actually, it replaces a string at any place of a step: action, 
-- condition, or response. 
replaceStringInStep :: Id -> String -> String -> Step -> Step
replaceStringInStep sid old new step = 
 if (sId step /= sid) 
  then step
  else step { action = rfn a, state = rfn s, response = rfn r }  
   where 
    a = action step
    s = state step
    r = response step  
    rfn = replaceString ("{"++old++"}") new
 
-- this is the generic function for replacing a string into a step (identified 
-- by 'sid'). It follows the SYB pattern. 
gReplaceParameterInScenario :: Id -> String -> String -> UseCaseModel -> UseCaseModel
gReplaceParameterInScenario sid fp ap = everywhere (mkT (replaceParameterInScenario sid fp ap)) 
      










