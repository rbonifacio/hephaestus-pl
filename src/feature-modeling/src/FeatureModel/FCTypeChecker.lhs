\section{Type Checker for Feature Configurations}

%if False
\begin{code}
module FeatureModel.FCTypeChecker where 

import FeatureModel.Types
import FeatureModel.FMTypeChecker

\end{code}
%endif 

The \texttt{validInstance} is the top most function for 
checking if a feature configuration (fc) is a valid instance 
of a feature model (fm). In order to do that, it is necessary to 
check if the root feature of those models are equal. Then, 
each constraint of the feature model must be considered by 
the configuration. 
 
\begin{code}
--validInstance :: FeatureModel -> FeatureConfiguration -> [ErrorMessage]
--validInstance fm fc = 
--  let f1 = fmTree fm
--      f2 = fcTree fc
--  in 
--   if (fnode f1 == fnode f2) then (checkType fm) ++ (checkFeatures f1 f2) ++ (checkConstraints fm fc)
--   else ["The root elements must be the same"]

validInstance' :: FeatureModel -> FeatureConfiguration -> Bool
validInstance' fm fc = 
 let fmExpression = foldAnd (fmToPropositionalLogic fm)
 in eval fc fmExpression 

-- checkType :: FeatureModel -> [ErrorMessage]
-- checkType fm = 
-- case fmTypeChecker fm of 
--  Success   -> [] 
--  Fail xs   -> [("Type checker of the feature model failed. " ++ show (xs))]
\end{code}

-- The type checker for a selected feature \texttt{fc}
-- verifies that it complies to the feature constraints 
-- defined in the feature model. Such a verification relies 
-- on the group type of the feature \texttt{fm}.

-- \begin{code}
-- checkFeatures :: FeatureTree -> FeatureTree -> ErrorList
-- checkFeatures FeatureError fc = [("Feature " ++ (fId fc) ++ " not expected")]
-- checkFeatures fm  FeatureError = [("Expecting feature " ++ (fId fm))]
-- checkFeatures fm fc = 
--  case (groupType fm) of 
--    BasicFeature -> checkBasicFeature fm fc
--    AlternativeFeature -> checkAlternativeFeature fm fc  
--    OrFeature -> checkOrFeature fm fc
-- \end{code}

-- We have defined specific functions for checking if a 
-- feature \texttt{fc} complies to the constraints of a feature 
-- \texttt{fm}, based on the \texttt{fm} type 
-- (if it is a mandatory, optional, alternative, and so on). Those 
-- functions are nor shown in this report.  

-- %if False
-- \begin{code}
-- -- check the constraints of a Basic feature
-- checkBasicFeature :: FeatureTree -> FeatureTree -> [ErrorMessage]
-- checkBasicFeature fm fc = 
--  (checkMandatoryFeatures fm fc) ++ (checkOptionalFeatures fm fc) 

-- -- check the constraints of a Mandatory feature
-- checkMandatoryFeatures :: FeatureTree -> FeatureTree -> [ErrorMessage]
-- checkMandatoryFeatures fm fc = 
--  foldr (++) [] [checkFeatures x (findChildFeature x fc) 
--                | x <- children fm
--                , fType x == Mandatory] 
   
-- -- check the constraints of Optional features
-- checkOptionalFeatures :: Feature -> Feature -> ErrorList
-- checkOptionalFeatures fm fc = (fms ++ fcs)
--  where
--   fms = foldr (++) [] [checkFeatures x y | x <- children fm, y <- children fc, x == y, fType x == Optional]
--   fcs = foldr (++) [] [checkFeatures FeatureError y | y <- children fc, (findChildFeature y fm) == FeatureError]

-- -- check the constraints of an Alternative feature
-- checkAlternativeFeature :: Feature -> Feature -> ErrorList
-- checkAlternativeFeature fm fc = 
--  case children fc of 
--   []     -> [("Exactly one child must be selected for feature " ++ (fId fm))]
--   [x]    ->  checkFeatures (findChildFeature x fm) x 
--   (x:xs) -> [("Exactly one child must be selected for feture " ++ (fId fm))] 

-- -- check the constraints of an Or feature
-- checkOrFeature :: Feature -> Feature -> ErrorList
-- checkOrFeature fm fc = 
--  case children fc of 
--   [] -> [("At least one child must be selected for feature " ++ (fId fm))]
--   xs -> foldr (++) [] [checkFeatures (findChildFeature x fm) x | x <- xs] 

-- -- check if the fc complies to the fm global constraints
-- checkConstraints :: FeatureModel -> FeatureConfiguration -> ErrorList
-- checkConstraints fm fc = [("Constraint " ++ (show c) ++ " not satisfied") 
--                          | c <- fmConstraints fm
--                          , not (evalConstraint fc c)
--                          ]

-- -- just an auxiliarly function for checking if errors where found
-- existError :: ErrorList -> Bool
-- existError [] = False
-- existError (x:xs) = True

\end{code}
%endif
