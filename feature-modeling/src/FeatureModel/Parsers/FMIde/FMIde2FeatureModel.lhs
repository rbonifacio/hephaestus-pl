\begin{code}

module FeatureModel.Parsers.FMIde.FMIde2FeatureModel where

import FeatureModel.Types
import FeatureModel.Parsers.FMIde.AbsFMIde


-- | Translate a FMIde Grammar into an instance of the 
--  FeatureModel data type.

grammarToFeatureModel :: Grammar -> FeatureModel
grammarToFeatureModel g@(TGrammar (p:ps) (cs)) = 
 case p of 
  -- we expecte a TBase production, since...
  TBaseProduction prod ts prodName -> FeatureModel { fmTree = (production2Feature g Mandatory BasicFeature p), 
                                                     fmConstraints = map constraint2Exp (cs)
                                                   }
  -- the production for the root feature must be a BaseProduction
  otherwise -> error "Expecting a base production for the root feature." 

constraint2Exp :: Expression -> FeatureExpression
constraint2Exp (BasicExp (Ident s)) = FeatureRef s
constraint2Exp (OrExp e1 e2) = (constraint2Exp e1) \/ (constraint2Exp e2)
constraint2Exp (AndExp e1 e2) = (constraint2Exp e1) /\ (constraint2Exp e2)
constraint2Exp (NotExp e1) = Not (constraint2Exp e1)
constraint2Exp (ImpliesExp e1 e2) = (constraint2Exp e1) |=> (constraint2Exp e2) 

production2Feature :: Grammar -> FeatureType -> GroupType -> Production -> FeatureTree
production2Feature g ft gt p@(TBaseProduction prod ts prodName) = baseProd2Feature g ft gt p
production2Feature g ft gt p@(TAltProduction prod os) = altProd2Feature g ft gt p

baseProd2Feature :: Grammar -> FeatureType -> GroupType -> Production -> FeatureTree
baseProd2Feature g ft gt (TBaseProduction prod ts prodName) = 
 case prodName of 
  -- productions in the form A : B C :: D are transformed into the feature D [B, C] 
  TProdName pn -> Root (Feature { fId = id2String pn,
                                  fName = id2String pn,
                                  fType = ft,
                                  groupType = gt,
                                  properties = []
                                 }
                       ) ([term2Feature g t | t <- ts])
   
  -- productions in the form A : B C :: _A are transformed into the feature A [B, C] 
  TProdNameL pn -> Root (Feature { fId = id2String (key prod),
                                   fName = id2String (key prod),
                                   fType = ft,
                                   groupType = gt,
                                   properties = []
                                 }
                        ) ([term2Feature g t | t <- ts])
 
  -- productions in the form A : B :: B_ are transformed into the feature A [B] 
  TProdNameR pn -> Root ( Feature { fId = id2String (key prod),
                                    fName = id2String (key prod),
                                    fType = ft,
                                    groupType = gt,
                                    properties = []
                                  }
                        ) (map (production2Feature g Mandatory AlternativeFeature) (findProduction (pn) g))   


altProd2Feature :: Grammar -> FeatureType -> GroupType -> Production -> FeatureTree
altProd2Feature g ft gt (TAltProduction prod os) = 
 Root (Feature { fId = id2String (key prod),
                 fName = id2String (key prod),
                 fType = ft,
                 groupType = gt,
               
                 properties = []
               }
      ) ([option2Feature g o | o <- os])

term2Feature :: Grammar -> Term -> FeatureTree
term2Feature g t = 
 let 
   def = term2FeatureDef t
   prods = findProduction (key t) g
 in case prods of 
  -- no production related to the term. then, we return a leaf
  []  -> Leaf Feature { 
              fId = id2String (key t),
              fName = id2String (key t),
              fType = fst def,
              groupType = snd def, 
              properties = []
         }

  -- one production related to the term. then, we should return one root 
  [x] -> production2Feature g (fst def) (snd def) x

  -- oops, more than one production with the same name
  (x:xs) -> error ("Expecting just one production labeled as: " ++ (id2String (key x))) 
 
term2FeatureDef :: Term -> (FeatureType, GroupType)
term2FeatureDef (TTerm x) = (Mandatory, BasicFeature)
term2FeatureDef (TOptionalTerm x) = (Optional, BasicFeature)
term2FeatureDef (TOrTerm x) = (Mandatory, OrFeature)
term2FeatureDef (TXorTerm x) = (Mandatory, AlternativeFeature)

option2Feature :: Grammar -> Option -> FeatureTree
option2Feature g opt = 
 let 
  prods = findProduction (key opt) g
 in case prods of 
  -- first case, there is no production 
  [] -> Leaf Feature { fId = id2String (key opt),
                       fName = id2String (key opt),
                       fType = Optional,
                       groupType = BasicFeature,
                       properties = []
                     }

  -- second case, there is one production
  [x] -> production2Feature g Optional BasicFeature x

  -- oops, more than one production with the same name 
  (x:xs) -> error ("Expecting just one production labeled as: " ++ (id2String (key x)))  
                  
findProduction :: Ident -> Grammar -> [Production]
findProduction i (TGrammar ps cs) = [p | p <- ps, ((key p) == i)] 

class Identifier a where 
 key :: a -> Ident

instance Identifier Production where
 key (TBaseProduction prod ts prodName) = key prod
 key (TAltProduction prod os) = key prod 

instance Identifier BaseProd where 
 key (TBaseProd i) = i

instance Identifier AltProd where 
 key (TAltProd i)  = i

instance Identifier ProdName where 
 key (TProdName i)  = i
 key (TProdNameL i) = i
 key (TProdNameR i) = i

instance Identifier Term where 
 key (TTerm i)         = i
 key (TOptionalTerm i) = i
 key (TOrTerm i)       = i
 key (TXorTerm i)      = i

instance Identifier Option where 
 key (TOption i) = i

id2String :: Ident -> String
id2String (Ident s) = s



\end{code}