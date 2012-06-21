\section{Type Checker for feature models}

The type checker for a feature model iterates over the 
features (starting from the root element) and checks if each 
feature is well typed. Additionally, 

\begin{itemize} 
\item The global constraints of the feature model must be well typed;  
\item Features should appear only once in the feature model; while  the equality property 
of a feature considers just the \emph{feature id} property; and
\item The feature model must be satisfiable, which means that at least one product configuration 
must satisfy the feature model relationships and constraints.    
\end{itemize}

%if False
\begin{code}
module FeatureModel.FMTypeChecker where 

import FeatureModel.Types

import Funsat.Types
import Funsat.Solver
import Funsat.Resolution

import List
import qualified Data.Set as Set

import Maybe
\end{code}
%endif

\begin{code}
fmTypeChecker :: FeatureModel -> CheckerResult
fmTypeChecker fm = 
 case errors of
  [] -> Success
  xs -> Fail { errorList = errors }
 where
  errors = if (e) == [] then checkIfSatisfiable else e
  e = (checkFeatureTree (fmTree fm)) ++ (checkConstraints fm) ++ (checkDuplications fm)   
  checkIfSatisfiable  = if (isSatisfiable fm) then [] else ["Feature model is not satisfiable."]
\end{code}

\subsection{Feature type checker}

The type checker for a feature checks if an \texttt{alternative} feature or 
an {or} feature had defined at least one child.  

\begin{code}
checkFeatureTree :: FeatureTree -> [ErrorMessage]
checkFeatureTree ftree = foldFTree (++) (checkFeature') (checkFeature') [] ftree 
 where
  checkFeature' ftree = 
   if ((groupType (fnode ftree)) /= BasicFeature) && (length (children ftree) == 0)  
    then [("Expecting at least one child for feature" ++ (fId (fnode ftree)))] 
    else []  
\end{code}

\subsection{Global constraints and satisfiability type checkers}

The type checker for the global constraints verifies that:

\begin{itemize}
 \item all references are made to features declared in the feature model; 
 \item an expression should not be inconsistent with the feature models restrictions--- eg.: 
       a constraint should not exclude a mandatory feature; and
 \item expressions should not be inconsistent among them--- for instance, the constraints 
       \texttt{A implies B} is inconsistent with \texttt{A implies not B}.
\end{itemize}

For that reason, we have decided to use a SAT solver for checking if the constraints 
of a feature model are satisfiable. The \texttt{fmSATSolver} function returns a configuration of 
features that satisfies the FM--- if it is satisfiable. Besides that, 
the \texttt{isSatisfiable} function returns True iff a feature model is 
satisfiable. 
 
\begin{code}
fmSATSolver :: FeatureModel -> (Solution, Stats, Maybe ResolutionTrace) 
fmSATSolver fm = solve1 (dimacsFormat (fmToCNFExpression fm))

isSatisfiable :: FeatureModel -> Bool 
isSatisfiable fm = 
 let 
  res = fmSATSolver fm
 in isSAT res

isSAT (x, y, z) = 
 case x of 
  Sat a -> True
  otherwise -> False  

\end{code}

  
Additionally, we have to check if the constraints refer to features declared 
in the feature model. 

\begin{code}
checkConstraints :: FeatureModel -> [ErrorMessage]
checkConstraints fm = 
 let 
  cnames = [(c,n) | c <- fmConstraints fm, n <- expNames c]
  fnames = [fId (fnode f) | f <- flatten (fmTree fm)]
  inames = [(c,n) | (c,n) <- cnames, notElem n fnames] --invalid references of the constraint 
 in ["Invalid feature " ++ n  ++ " found in the constraint " ++ show (c) | (c,n) <- inames] 
 
expNames :: FeatureExpression -> [String] 
expNames (And e1 e2)    = (expNames e1) ++ (expNames e2)
expNames (Or  e1 e2)    = (expNames e1) ++ (expNames e2)
expNames (Not e1)       = expNames e1
expNames (FeatureRef e) = [e]
expNames otherwise    = []  

\end{code}

\subsection{Duplication type checker}

In order to check for duplicated features, we just need 
to compare if the number of features of the feature model 
\texttt{fm} is equal to the number of features after excluding 
any duplication. If it is not the case, there are duplicated 
features.  

 
\begin{code}
checkDuplications :: FeatureModel -> [ErrorMessage]
checkDuplications fm = 
 let 
  fnames = [fId (fnode f) | f <- flatten (fmTree fm)]
  dupps  = [f | f <- fnames, length (filter (==f) fnames) > 1]
 in ["Feature " ++ f ++ " is duppliecated." | f <- dupps] 
 
\end{code}

\section{Detecting bad smells}

In this section we present several functions for detecting 
feature models bad smells. 

\subsection{Missing alternatives}
this bad smell occurs when a feature A,
defined as an \emph{Or Feature} or an \emph{Alternative Feature}, 
declares only one child B. Detecting this bad smell might reveal a
controversial decision of creating a group of alternatives,
instead of declaring a mandatory relationship between A and
B. 

\begin{code}

missingAlternatives :: FeatureModel -> [Feature]
missingAlternatives fm = [ fnode f 
                         | f <- flatten (fmTree fm)
                         , groupType (fnode f) /= BasicFeature 
                         , length (children f) < 2
                         ]

\end{code}

\subsection{Constraint imposing alternative}

This bad smell occurs when a global constraint obligates
the selection of an Alternative Feature. Since a child must
always be selected, the other children of this relationship
can never be selected. Detecting this kind of bad smell might
reveal constraints that are inconsistent with the feature model
relationships.

The following types of constraints originate this kind of 
bad smell. 

\begin{code}
checkConstraintImposingAlternative :: FeatureModel -> [(FeatureExpression, Feature)]
checkConstraintImposingAlternative fm = 
 let cs = [(c,constraintImposingAlternative c (fmTree fm)) | c <- fmConstraints fm]
 in [(c,fromJust f) | (c,f) <- cs, isJust f]  
\end{code}
                                      
{\bf Reference to an alternative feature}

\begin{code}
constraintImposingAlternative :: FeatureExpression -> FeatureTree -> Maybe Feature
constraintImposingAlternative (FeatureRef r) ftree = 
 let f = head [fnode x | x <- (flatten ftree), (fId (fnode x)) == r]
 in if f `elem` (alternativeChildren ftree) 
     then Just f 
     else Nothing
\end{code}

{\bf Essential feature implying alternative feature}

\begin{code}
constraintImposingAlternative (Or (Not (FeatureRef r1)) (FeatureRef r2)) ftree = 
 let 
  f1 = head [fnode x | x <- (flatten ftree), (fId (fnode x)) == r1]
  f2 = head [fnode x | x <- (flatten ftree), (fId (fnode x)) == r2]
 in if (f1 `elem` (essentialFeatures ftree)) && (f2 `elem` alternativeChildren ftree) 
     then Just f2 
     else Nothing
\end{code}

{\bf Other cases are not considered as Constraint imposing alternative} 

\begin{code}
constraintImposingAlternative otherwise ftree = Nothing
\end{code}


\subsection{Constraint imposing optional feature}

This bad smells occurs when a constraint changes the 
semantics of a relationship, making it more restrictive. In 
that cases, it would be better to change the relationship in the 
feature model, since the graphical representation is easier to understand. 

The following types of constraints originate this kind of 
bad smell. 

{\bf Reference to an Optional feature or reference to a child of an Or feature}

\begin{code}
checkConstraintImposingOptional :: FeatureExpression -> FeatureTree -> Maybe Feature
checkConstraintImposingOptional (FeatureRef r) ftree = 
 let f = head [fnode x | x <- (flatten ftree), (fId (fnode x)) == r]
 in if (f `elem` (orChildren ftree ++ optionalFeatures ftree))  
     then Just f 
     else Nothing 
\end{code}

\subsection{Superimposing optional feature}

This bad smells occurs when an optional feature, 
a child of an alternative feature, or a child of an or feature 
is superimposed by a constraint. Basically, in order to identify 
this bad smell we have just to introduce a new constraint 
with the negation of these features. If the feature model 
becomes insatisfiable, the feature is superimposed by a constraint. 

\begin{code}
superimposedOptional :: FeatureModel -> [Feature]
superimposedOptional fm = 
 let ftree = fmTree fm
  in 
   [ f 
   | f <- nub (optionalFeatures ftree ++ alternativeChildren ftree ++ orChildren ftree)
   , not (isSatisfiable (addConstraint fm (Not (ref f))))
   ]

\end{code}

\subsection{Dead features}

This bad smell occurs when a feature, due to a global
constraint, could never be selected in any of the valid
instances of a feature model. It may happen when 
overconstraining the model, making it difficult to understand,
hence, suitable for introducing errors. 


\begin{code}
checkDeadFeatures :: FeatureModel -> [Feature]
checkDeadFeatures fm = [ fnode f 
                       | f <- flatten (fmTree fm) 
                       , fType (fnode f) == Optional
                       , not (isSatisfiable (addConstraint fm (ref (fnode f))))
                       ]


type BadSmell = String 

findBadSmells :: FeatureModel -> [BadSmell]
findBadSmells fm = 
 if ((fmTypeChecker fm == Success) && isSatisfiable fm)
  then 
   ["Expecting at least 2 children in feature " ++ show (f) | f <- missingAlternatives fm] ++
   ["Feature " ++ show f ++ " is a dead feature" | f <- checkDeadFeatures fm] ++ 
   ["Constraint " ++ show c ++ " impose alternative feature " ++ show f | (c,f) <- checkConstraintImposingAlternative fm]
 else
  ["The feature model is either incorrect or inconsistent (unsatisfiable)"]


addConstraint :: FeatureModel -> FeatureExpression -> FeatureModel
addConstraint fm exp = fm { fmConstraints = exp : cs}
 where cs = fmConstraints fm 
 
\end{code}