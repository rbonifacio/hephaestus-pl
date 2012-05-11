
%
% This is the Literate Haskell that defines the main module of the 
% FeatureModeling library.
% 
% author: Rodrigo Bonifacio - rbonifacio@computer.org
%
% 2008/2009
%


\section{FeatureModel Data Types}

This module defines the main concepts (data types such as 
feature model, features, constraints, and so on) related 
to the feature modeling context. Moreover, this module  
makes available functions for checking if a product configuration 
is a valid instance of a feature model.  
	
%if False
\begin{code}
{-# OPTIONS -fglasgow-exts #-}
module FeatureModel.Types where 

import BasicTypes

import Funsat.Types

import qualified Data.Set as Set

import Data.Generics
import List
import Maybe


\end{code}
%endif 

\subsection{Data types}

First of all, we have to declare some data types and synonimous 
for feature modeling, which represent the main 
concepts related to the feature modeling context. Remember that, in Haskell, 
the \texttt{type} construct is used for definining new names for 
existing types.On the other hand, new data types are defined using 
the \texttt{data} construct. New data types might have different functions
for building types. For instance, the data type \texttt{CheckerResult} has 
two constructors: \texttt{Success} and \texttt{Fail}.   

\begin{code}
type Root          = Feature            
type Property      = (String, String)     
type ErrorMessage  = String
data CheckerResult = Success | Fail { errorList :: [ErrorMessage] } deriving (Show,Eq)
\end{code}

The \texttt{FeatureType} data type defines if a feature is required or 
not in a feature configuration. Therefore, according to its type, 
a feature can be either an \emph{optional} or a \emph{mandatoty} feature.

\begin{code}
data FeatureType  = Optional | Mandatory 
 deriving (Show, Eq, Typeable, Data)
\end{code}

Features are also classified according to the semantics of the 
valid configurations of their children. For instance, \texttt{basic} features might 
not have children. On the other hand, \texttt{alternative} and \texttt{or} features 
{\bf must have} children. The difference is that, when selected,  
an \texttt{alternative} feature require exactly one child to be selected. Differently,  
an \texttt{or} feature, when selected in a product configuration, 
requires that {\bf at least one of its child} must  be selected. 

\begin{code}
data GroupType = BasicFeature | AlternativeFeature | OrFeature 
 deriving (Show, Eq, Typeable, Data)
\end{code}

Now we can define the feature model data type, which is basically the root of a rose tree--- whose nodes are 
features--- pluss a list of global constraints. Similarly, a feature configuration, 
or selection of features, is also a tree of features--- although the feature configuration 
data type has just a reference to a root feature. 

\begin{code}
data FeatureTree = Leaf Feature | Root Feature [FeatureTree]
 deriving (Show, Typeable, Data)

fnode :: FeatureTree -> Feature
fnode (Leaf f) = f
fnode (Root f fs) = f

children :: FeatureTree -> [Feature]
children (Leaf f) = []
children (Root f fs) = map fnode fs 
  
data FeatureModel = FeatureModel {
	fmTree :: FeatureTree,
        fmConstraints :: [FeatureExpression]
} deriving (Show, Typeable, Data)

data FeatureConfiguration = FeatureConfiguration {
	fcTree :: FeatureTree
} deriving (Show, Typeable, Data)

foldFTree::  (b -> b -> b) -> (FeatureTree -> b) -> (FeatureTree -> b) -> b -> FeatureTree ->  b
foldFTree f1 f2 f3 f4 (Leaf f)  = f2 (Leaf f)
foldFTree f1 f2 f3 f4 (Root f fs) = f1 (f3 (Root f fs)) (foldr (f1) f4 [foldFTree f1 f2 f3 f4 x | x <- fs])  
\end{code}

A feature is either a concrete description of a product capability (with a 
name, type, children and so on); or an error representation of a feature, which 
is a usefull representation for reporting some kinds of errors. 

\begin{code}
data Feature = Feature {
	fId :: Id, 
	fName :: Name, 
	fType :: FeatureType,
	groupType :: GroupType,
	properties :: [Property]
 } | FeatureError    
 deriving (Typeable, Data)
\end{code}


Feature expressions are written in propositional formula, with the NOT, AND, and OR connectors. 
Expressions are usefull for specifying constraints, as state above, but are also usefull in the 
definition of the configuration knowledge. In this later case, feature expressions are necessary for 
dealing with some types of feature interactions. 

\begin{code}
data FeatureExpression = ConstantExpression Bool 
                       | FeatureRef Id 
                       | Not FeatureExpression 
                       | And FeatureExpression FeatureExpression 
                       | Or FeatureExpression FeatureExpression
 deriving (Typeable, Data) 
            
-- the constant expressions for representing True and False
expTrue = ConstantExpression True
expFalse = ConstantExpression False

-- check if a expression is an implies expression
isImpliesExpression :: FeatureExpression -> Bool
isImpliesExpression (Or (Not e1) (e2)) = True
isImpliesExpression otherwise = False
\end{code}

Finally, we have defined some \emph{syntactic sugars} for building 
expressions, such as: 

\begin{itemize}
 \item operators, such as $p \Rightarrow q$ that means $Implies p q$
 \item generating an And expression from a list of expressions
 \item generating an Or expression from a list of expressions 
 \item generating an FeatureRef expression from a feature 
\end{itemize}

\begin{code}
(|=>) :: FeatureExpression -> FeatureExpression -> FeatureExpression
e1 |=> e2 = Or (Not e1) e2  

(<=>) :: FeatureExpression -> FeatureExpression -> FeatureExpression
e1 <=> e2 = And (Or (Not e1) e2) (Or (Not e2) e1) 

(/\) :: FeatureExpression -> FeatureExpression -> FeatureExpression
e1 /\ e2 = And e1 e2 

(\/) :: FeatureExpression -> FeatureExpression -> FeatureExpression
e1 \/ e2 = Or e1 e2

foldAnd xs = simplifyExpression (foldr And (expTrue) xs)
foldOr xs  = simplifyExpression (foldr Or  (expFalse) xs)

ref :: Feature -> FeatureExpression
ref f = FeatureRef (fId f)

\end{code}

\subsection{Feature Models to Propositional Logic}

The constraints defined in a whole feature model migh be translated to 
propositional logic, which is a useful representation for reasoning about 
feature models. This translation consists of:

\begin{itemize}
 \item create an expression to the root feature, stating that it must be present in 
       all members of the product line; and 
 \item translate each feature, in the feature model, to a corresponding propositional 
       logic expression.  
\end{itemize}
 
The corresponding propostional expression for a feature \emph{f} depends on  
the group type of the feature. For a \emph{Basic Feature}, we state that \emph{f} implies 
all of its mandatory children; for an \emph{Alternative Feature}, we state that \emph{f}
implies at least one child to be selected; and, finally, for an \emph{Or Feature}, we 
state that \emph{f} implies one, and only one, child to be selected. 
The result is a list of \emph{feature expressions} that {\bf must} be 
satisfied in order to consider a product as a valid instance of the feature model.  

\begin{code}
fmToPropositionalLogic :: FeatureModel -> [FeatureExpression]
fmToPropositionalLogic fm = rootProposition ++ ftPropositions ++ csPropositions 
 where 
  (Root f fs) = fmTree fm
  ftPropositions  = foldFTree (++) (\(Leaf f) -> []) (featureToPropositionalLogic) [] (Root f fs)
  csPropositions  = fmConstraints fm
  rootProposition = [ref f]

featureToPropositionalLogic :: FeatureTree -> [FeatureExpression]
featureToPropositionalLogic  ftree = 
 let 
  f  = fnode ftree
  cs = children ftree 
 in (
      case groupType f of
       BasicFeature       -> [(ref f) |=> (ref c) | c <- cs, fType c == Mandatory]
       OrFeature          -> [(ref f) |=> (foldOr [ref x | x <- cs])]  
       AlternativeFeature -> [(ref f) |=> (foldOr [xor x (delete x cs) | x <- cs])] 
    ) ++ [(ref c) |=> (ref f) | c <- cs]  

xor f [] = ref f
xor f xs = And (ref f) (foldAnd [Not (ref x) | x <- xs])
\end{code}

Then, it was also necessary to convert the feature expressions to the Conjunctive 
Normal Form (CNF), since most of the available SAT solvers expect 
sentences in CNF. \emph{FunSAT}, the SAT solver used in this librar, is available at 
the Hackage repository.
\begin{code}
fmToCNFExpression :: FeatureModel -> FeatureExpression 
fmToCNFExpression fm = 
 let
  fmExpressions = fmToPropositionalLogic fm
 in toCNFExpression (foldAnd fmExpressions) 

fmToTseitinEncode :: FeatureModel -> FeatureExpression
fmToTseitinEncode fm = 
 let
  fmExpressions = fmToPropositionalLogic fm
 in toTseitinEncode (foldAnd fmExpressions)
 
\end{code}

Auxiliarly funcions were defined for converting feature expressions 
to both CNF and Dimacs CNF format. These functions are not
shown in this report. Besides that, the \texttt{eval} function checks if a 
feature configuration (\texttt{fc}) satisfies the constraints represented 
in the feature expression (\text{exp}). As a consequence, it is also 
useful for checking if \texttt{fc} is a valid instance of a feature model. 

We provide two implementations for converting a propositional formula to CNF. The 
\texttt{Tseitin} algorithm have a linear time, although it is not so clear. 
For real feature models, the Tseitin implementation must be used.

%if False
\begin{code}
toCNFExpression :: FeatureExpression -> FeatureExpression
toCNFExpression (And e1 e2)    = And (toCNFExpression e1) (toCNFExpression e2)
toCNFExpression (Or e1 e2)     = distributeAndOverOr e1 e2
toCNFExpression (Not e1)       = moveNotInwards e1
toCNFExpression (FeatureRef f) = (FeatureRef f) 

distributeAndOverOr :: FeatureExpression -> FeatureExpression -> FeatureExpression  
distributeAndOverOr (And x y) e2 = And (toCNFExpression (Or x e2)) (toCNFExpression(Or y e2)) 
distributeAndOverOr e1 (And x y) = And (toCNFExpression(Or e1 x)) (toCNFExpression(Or e1 y))
distributeAndOverOr e1 e2 = distributeAndOverOr' a b 
 where 
  distributeAndOverOr' (And x y) e = toCNFExpression (Or (And x y) e)
  distributeAndOverOr' e (And x y) = toCNFExpression (Or e (And x y))
  distributeAndOverOr' x y = Or (toCNFExpression x) (toCNFExpression  y) 
  a = toCNFExpression e1
  b = toCNFExpression e2
 
moveNotInwards :: FeatureExpression -> FeatureExpression
moveNotInwards (And x y) = Or  (toCNFExpression (Not x)) (toCNFExpression (Not y))
moveNotInwards (Or x y)  = And (toCNFExpression (Not x)) (toCNFExpression (Not y))
moveNotInwards (Not x)   = toCNFExpression x
moveNotInwards e         =  Not e  

type Gate = Integer

toTseitinEncode :: FeatureExpression -> FeatureExpression
toTseitinEncode (Or e1 e2) =
 let
   a1 = newRef [1] 
   a2 = newRef [2]
 in foldAnd ([Or a1 a2] ++ (toTseitinEncode' [1] e1) ++ (toTseitinEncode' [2] e2))

toTseitinEncode (And e1 e2) =
 let
   a1 = newRef [1] 
   a2 = newRef [2]
 in foldAnd ([And a1 a2] ++ (toTseitinEncode' [1] e1) ++ (toTseitinEncode' [2] e2)) 
 
toTseitinEncode (Not e1) = 
 let a1 = newRef [1] 
 in foldAnd( [Not a1] ++ (toTseitinEncode' [1] e1))

toTseitinEncode e = e   

-- toTseitinEncode' _  (FeatureRef e) = []

toTseitinEncode' gs (Or e1 e2) = 
 let 
  gl = gs ++ [1]
  gr = gs ++ [2]
  w  = newRef gs
  w1 = newRef gl
  w2 = newRef gr
 in  [And (Or (Not w) (Or w1 w2) ) (And (Or w (Not w1)) (Or w (Not w2)))] ++ 
     (toTseitinEncode' gl e1) ++
     (toTseitinEncode' gr e2)

toTseitinEncode' gs (And e1 e2) = 
 let
  gl = gs ++ [1]
  gr = gs ++ [2]
  w  = newRef gs
  w1 = newRef gl
  w2 = newRef gr
 in [And (Or (Not w) w1) (And (Or (Not w) w2) (Or w (Or (Not w1) (Not w2))))] ++
    (toTseitinEncode' gl e1) ++
    (toTseitinEncode' gr e2)
    
toTseitinEncode' gs (Not e1) = 
 let 
  gl = gs ++ [1] 
  w  = newRef gs
  w1 = newRef gl
 in [And (Or (Not w) (Not w1)) (Or w w1) ] ++ (toTseitinEncode' gl e1)

toTseitinEncode' gs  otherwise = []
     
newRef :: [Gate] -> FeatureExpression
newRef gs = FeatureRef (foldl (++) "g" [show g | g <- gs])
 
dimacsFormat :: FeatureExpression -> CNF 
dimacsFormat exp = 
 let 
  vars = getVars exp
  cs = map (expToLiterals vars) (getClauses exp)
 in CNF {
   numVars = length vars,
   numClauses = length cs,
   clauses =  Set.fromList cs
 } 

data FMSummary = FMSummary {
  nfeatures :: Int,
  nconstraints :: Int
} deriving (Show)

summary :: FeatureModel -> FMSummary 
summary fm = 
 let 
   e = fmToPropositionalLogic fm
   r = fmTree fm
 in FMSummary (length (flatten r)) (length (e))

getVars :: FeatureExpression -> [FeatureExpression] 
getVars (And exp1 exp2)  = nub ((getVars exp1) ++ (getVars exp2))
getVars (Or  exp1 exp2)  = nub ((getVars exp1) ++ (getVars exp2))
getVars (Not exp1)       =  getVars exp1
getVars (FeatureRef f) = [(FeatureRef f)]
getVars otherwise      = []

getClauses :: FeatureExpression -> [FeatureExpression]
getClauses (And exp1 exp2) = (getClauses exp1) ++ (getClauses exp2)
getClauses (Or  exp1 exp2) = [Or exp1 exp2]
getClauses (Not exp1)      = [Not exp1]
getClauses (FeatureRef f)  = [FeatureRef f]
getClauses otherwhise      = [] 

expToLiterals :: [FeatureExpression] -> FeatureExpression -> Clause
expToLiterals fs e = map intToLiteral (expToLiterals' fs e)
 where
  expToLiterals' fs (Or  exp1 exp2) = (expToLiterals' fs exp1) ++ (expToLiterals' fs exp2) 
  expToLiterals' fs (Not exp1)      = map (*(-1)) (expToLiterals' fs exp1)
  expToLiterals' fs (FeatureRef f)  = 
   if isJust (elemIndex (FeatureRef f) fs)
    then [fromJust(elemIndex (FeatureRef f) fs) + 1] 
    else []
  expToLiterals' fs otherwise = []
  intToLiteral x = L { unLit = x }
\end{code}
%endif

\begin{code}
eval :: FeatureConfiguration -> FeatureExpression -> Bool
eval config (FeatureRef f) = elem f [fId (fnode x) | x <- flatten (fcTree config)]
eval config (Not e) = not (eval config e)
eval config (And e1 e2) = (eval config e1) && (eval config e2)
eval config (Or e1 e2) = (eval config e1) || (eval config e2)
eval _ (ConstantExpression e) = e

\end{code}



\subsection{Searching and traversing feature models}

Now, we present several functions for searching and traversing 
feature models. First of all, the \texttt{plainFeature} function 
translates a feature tree into a flat structure (a list) of 
features. It is just an auxiliarly function that simplifies 
the definition of functions for searching features. 

\begin{code}
flatten :: FeatureTree -> [FeatureTree]
flatten ftree = foldFTree (++) unit unit [] ftree 
 where 
  unit ftree = [ftree]

\end{code}

Using the \texttt{flatten} function, we can easly define functions 
for searching a feature \texttt{f1} (or for checking if it exists) 
in a feature tree \texttt{f2}. 
 
\begin{code}
featureExists :: Feature -> FeatureTree -> Bool 
featureExists  f1 ft = elem f1 (map fnode (flatten ft))

findFeature :: Feature -> FeatureTree -> Feature
findFeature f1 ft = findFeature' f1 (map fnode (flatten ft))  
 where 
  findFeature' f1 [] = FeatureError  
  findFeature' f1 (x:xs) = if (f1 == x) then x else findFeature' f1 xs
\end{code}

Bellow we present a few functions for peforming more specific 
queries on feature models: 

\begin{itemize}
 \item The \texttt{allChildrenExists} function returns True if all children 
  elements of the feature f1 are also defined as children of the feature f2.
 \item The \texttt{findChildFeature} returns a children of f2 which is 
  equals to f1; where equality is based on the feature ids. 
 \item The \texttt{findFeatureInList} returns the feature in (x:xs) that is 
  equals to f1. 
\end{itemize}

Notice that these functions are useful for checking if a feature 
configuration belongs to (or is a valid instance of) a feature 
model. 

\begin{code}
allChildrenExists :: FeatureTree -> FeatureTree -> Bool 
allChildrenExists f1 f2 = and [elem x (cs f2) | x <- (children f1)]
 where 
  cs (Root f fs) = map fnode fs 
  cs (Leaf f) = []

findChildFeature :: Feature -> FeatureTree -> Feature
findChildFeature f1 ft = findFeatureInList f1 (children ft)

findFeatureInList :: Feature -> [Feature] -> Feature
findFeatureInList f [] = FeatureError 
findFeatureInList f (x:xs) = if (f == x) then x else findFeatureInList f xs
\end{code}

\subsection{Accessors for feature options and properties}
Still related to the feature model data type, we have 
developed several functions for accessing:

\begin{itemize}
 \item the selected options of features; and 
 \item the attached properties of a feature.
\end{itemize}

The \texttt{featureOptions} function retrieves the selected options 
of an \emph{Alternative feature} or \emph{Or feature}. An error is 
reported if this function is called with a \emph{Basic feature} as 
argument. Besides that, the \texttt{featureOptionsValues} function retrieves 
a string representation of the selected values of an \emph{Alternative feature} 
or \emph{Or feature}.

\begin{code}
featureOptions :: FeatureTree -> [Feature]
featureOptions ft = children ft
  -- if groupType (fnode ft) == BasicFeature 
  --then error "The function featureOptions can not be applied to basic features"
  -- else children ft

featureOptionsValues :: FeatureTree -> [String]
featureOptionsValues ft = map fName (featureOptions ft)

\end{code}

If the application developer is interested in a property of the 
selected options, the \texttt{featureOptionsPropertyValue} function can 
be used, instead of the two functions shown immediately above. Again, 
this function does not expect a \emph{Basic feature} as the first argument. Otherwise, 
an error is reported.  

\begin{code}
featureOptionsPropertyValue :: String -> FeatureTree -> [String]
featureOptionsPropertyValue pid ft = 
 [snd y | x <- (featureOptions ft), y <- (properties x), fst y == pid]  
\end{code}
 

%if False
\begin{code}
-- 
-- Eq instance definition is (or are)
-- placede at this point.
--
instance Eq Feature where 
 Feature id1 _ _ _ _  == Feature id2 _ _ _ _  = id1 == id2
 FeatureError == FeatureError = True
 FeatureError == _ = False
 _ == FeatureError = False 
 
instance Eq FeatureExpression where 
 FeatureRef id1 == FeatureRef id2 = id1 == id2
 FeatureRef id1 == _ = False  

 ConstantExpression e1 == ConstantExpression e2 = e1 == e2
 ConstantExpression e1 == _ = False

 Not fExp1 == Not fExp2 = fExp1 == fExp2 
 Not fExp1 == _ = False

 And fExp1 fExp2 == And fExp3 fExp4 = 
  ((fExp1 == fExp3) && (fExp2 == fExp4)) || ((fExp1 == fExp4) && (fExp2 == fExp3))
 And fExp1 fExp2 == _ = False
 
 Or fExp1 fExp2 == Or fExp3 fExp4 = 
  ((fExp1 == fExp3) && (fExp2 == fExp4)) || ((fExp1 == fExp4) && (fExp2 == fExp3))
 Or fExp1 fExp2 == _ = False

-- 
-- Show instance definition are 
-- placed in this point.
--
instance Show Feature where 
 show FeatureError = "Feature error" 
 show f = fId f

instance Show FeatureExpression where
  show (FeatureRef id1) = show id1
  show (And exp1 exp2) = "And (" ++ (show exp1) ++ ", " ++ (show exp2) ++ ")"
  show (Or exp1 exp2) = "Or (" ++ (show exp1) ++ ", " ++ (show exp2) ++ ")"
  show (Not exp1) = "Not (" ++ (show exp1)  ++ ")"
  show (ConstantExpression True) = "True"
  show (ConstantExpression False) = "False" 

-- and expression simplifcations
simplifyExpression :: FeatureExpression -> FeatureExpression
simplifyExpression (And e1 e2) = simplifyAnd e1 e2 
simplifyExpression (Or e1 e2)  = simplifyOr e1 e2
simplifyExpression (Not e)     = simplifyNot e
simplifyExpression (FeatureRef f)        = FeatureRef f 
simplifyExpression (ConstantExpression b) = ConstantExpression b

simplifyAnd :: FeatureExpression -> FeatureExpression -> FeatureExpression
simplifyAnd e1 e2  
 | (e1 == expFalse) || (e2 == expFalse) = expFalse
 | e1 == expTrue = simplifyExpression e2
 | e2 == expTrue = simplifyExpression e1
 | otherwise = And (simplifyExpression e1) (simplifyExpression e2)


simplifyOr :: FeatureExpression -> FeatureExpression -> FeatureExpression
simplifyOr e1 e2  
 | (e1 == expTrue) || (e2 == expTrue) = expTrue
 | e1 == expFalse = simplifyExpression e2
 | e2 == expFalse = simplifyExpression e1
 | otherwise = Or (simplifyExpression e1) (simplifyExpression e2)

simplifyNot :: FeatureExpression -> FeatureExpression 
simplifyNot e 
 | e == expTrue = expFalse
 | e == expFalse = expTrue
 | otherwise = Not (simplifyExpression e)

essentialFeatures :: FeatureTree -> [Feature]
essentialFeatures ftree = 
 foldFTree (++) (filterMandatory) (filterMandatory) [] ftree
 where 
  filterMandatory ftree = if isMandatory (fnode ftree) 
   then [fnode ftree] 
   else [] 

alternativeChildren :: FeatureTree -> [Feature]
alternativeChildren ftree = concat [children f | f <- flatten ftree, isAlternative (fnode f)]

orChildren :: FeatureTree -> [Feature]
orChildren ftree = concat [children f | f <- flatten ftree, isOrFeature (fnode f)]

optionalFeatures :: FeatureTree -> [Feature]
optionalFeatures ftree = [fnode f | f <- flatten ftree, fType (fnode f) == Optional]

isMandatory :: Feature -> Bool
isMandatory FeatureError = False
isMandatory f = (fType f) == Mandatory

isAlternative :: Feature -> Bool
isAlternative FeatureError = False
isAlternative f = (groupType f) == AlternativeFeature

isOrFeature :: Feature -> Bool
isOrFeature FeatureError = False
isOrFeatire f = (groupType f) == OrFeature

\end{code}
%endif
