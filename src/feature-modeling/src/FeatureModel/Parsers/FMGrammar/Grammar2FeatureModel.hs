module FeatureModel.Parsers.FMGrammar.Grammar2FeatureModel (grammarToFeatureModel) where 

import FeatureModel.Types
import FeatureModel.Parsers.FMGrammar.AbsFMGrammar

grammarToFeatureModel :: FMGrammar -> FeatureModel
grammarToFeatureModel (FMGrammar (p:ps) cs) = FeatureModel root ac
  where
   fr  = Feature { fId = (key p) , fName = (key p), fType = Mandatory, groupType = BasicFeature, properties = [] }
   fc = map (child2FeatureTree ps) (childrenOfProduction p)  
   root = (Root fr fc)
   ac = map constraint2FeatureExpression cs 

child2FeatureTree :: [Production] -> Child -> FeatureTree
child2FeatureTree ps c = 
 let 
  (t, n) = case c of 
                (MandatoryChild i) -> (Mandatory, key i) 
                (OptionalChild i) -> (Optional, key i) 
  prods = findProduction (ps) (key c)
  f = Feature { fId = n, fName = n, fType = t, groupType = BasicFeature, properties = []} 
 in case prods of 
     [] -> Leaf f
     [(BaseProduction i cs)] -> Root f (map (child2FeatureTree ps) cs) 
     [(OrProduction i os)]   -> Root (f { groupType = OrFeature }) (map (option2FeatureTree) os)
     [(XorProduction i os)]  -> Root (f { groupType = AlternativeFeature}) (map (option2FeatureTree) os)            
     otherwise -> error ("Expecting just one production with the name " ++ (show (key c)))

option2FeatureTree :: Option -> FeatureTree 
option2FeatureTree (Option i) = 
 let f = Feature { fId = (key i), fName = (key i), fType = Optional, groupType = BasicFeature, properties = []} 
 in Leaf f

constraint2FeatureExpression :: Expression -> FeatureExpression
constraint2FeatureExpression (BasicExp (Ident s)) = (FeatureRef s)
constraint2FeatureExpression (ImpliesExp e1 e2) = (constraint2FeatureExpression e1) |=> (constraint2FeatureExpression e2)
constraint2FeatureExpression (IffExp e1 e2) = (constraint2FeatureExpression e1) <=> (constraint2FeatureExpression e2)
constraint2FeatureExpression (AndExp e1 e2) = (constraint2FeatureExpression e1) /\ (constraint2FeatureExpression e2)
constraint2FeatureExpression (OrExp e1 e2) = (constraint2FeatureExpression e1) \/ (constraint2FeatureExpression e2)
constraint2FeatureExpression (NotExp e1) = Not (constraint2FeatureExpression e1)

-- some auxilliarly classes and functions. 

findProduction :: [Production] -> String -> [Production] 
findProduction ps i = [p | p <- ps , key (p) == i]

childrenOfProduction :: Production -> [Child] 
childrenOfProduction (BaseProduction i cs) = cs

class Identifier a where 
 key  :: a -> String

instance Identifier Ident where 
 key (Ident s) = s

instance Identifier Production where 
 key (BaseProduction i c) = key i 
 key (OrProduction i o)   = key i
 key (XorProduction i o)  = key i

instance Identifier Child where 
 key (MandatoryChild i) = key i 
 key (OptionalChild i)  = key i 
