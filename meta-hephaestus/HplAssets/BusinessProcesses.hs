module HplAssets.BusinessProcesses (
  transformBpm,
  emptyBpm
) where

import BasicTypes
import List   -- for function "nub" used in evaluateBeforeAdvice
import Maybe  -- for function "mapMaybe" used in the conversion of a triplet in Transition data type 
import HplAssets.BPM.Types
import FeatureModel.Types
-- *******************************************************
import HplProducts.TestTypes -- where is defined the data types SPLModel and InstanceModel
-- *******************************************************

emptyBpm :: BusinessProcessModel -> BusinessProcessModel
emptyBpm bp = bp { processes = [] }
 
transformBpm :: BusinessProcessTransformation -> SPLModel -> InstanceModel -> InstanceModel
transformBpm (SelectBusinessProcess id) spl product = product { bpm = selectBusinessProcess id (splBpm spl) (bpm product)}
transformBpm (BindParameterBpm np vp) _ product = product { bpm = bindParameter np vp (bpm product)}
transformBpm (EvaluateAdvice id) spl product = product { bpm = evaluateAdvice id (splBpm spl) (bpm product)}

-----------------------------------------

selectBusinessProcess :: Id -> BusinessProcessModel -> BusinessProcessModel -> BusinessProcessModel
selectBusinessProcess id spl product = 
 let bps = [bp | bp <- (processes spl), (pid bp == id), bp `notElem` (processes product)]
 in case bps of
   [bp] -> BPM ([bp] ++ (processes product))
   [] -> product


-----------------------------------------

bindParameter :: Name -> Value -> BusinessProcessModel -> BusinessProcessModel 
bindParameter np vp bpm = 
  BPM (
       map (bindParameter' np vp) (processes bpm)
      )

bindParameter' :: Name -> Value -> BusinessProcess -> BusinessProcess
bindParameter' np vp bp = 
 BusinessProcess {
  pid = pid bp,
  ptype = ptype bp,
  objects = (map (applyParm np vp) (objects bp)),
  transitions = transitions bp
}
 
applyParm :: Name -> Value -> FlowObject -> FlowObject
applyParm np vp fo =
 FlowObject {
    id' = idflow fo,
    type' = typeFlow fo,
    annotations' = annotations fo,
    parameters' = ([(np,vp) | (i,j) <- (parameters fo), np == i ] ++ 
                   [(i,j)   | (i,j) <- (parameters fo), np /= i ])
}

-----------------------------------------

evaluateAdvice :: Id -> BusinessProcessModel -> BusinessProcessModel -> BusinessProcessModel
evaluateAdvice advId spl product = 
 let advs = [a | a <- (processes spl), (pid a) == advId]
 in case advs of 
      [adv] -> BPM (map (evaluateAdvice' adv) (processes product))
      otherwise -> product

evaluateAdvice' :: BusinessProcess -> BusinessProcess -> BusinessProcess
evaluateAdvice' adv bp =
 let exist = [obj | obj <- (objects bp), (obj `matches` (pointcut adv))] /= []
 in case ptype adv of 
   BasicProcess      -> bp 
   (Advice After  _) -> if exist then (evaluateAfterAdvice adv bp) else bp
   (Advice Before _) -> if exist then (evaluateBeforeAdvice adv bp) else bp
   (Advice Around _) -> if exist then (evaluateAroundAdvice adv bp) else bp

evaluateAfterAdvice :: BusinessProcess -> BusinessProcess -> BusinessProcess
evaluateAfterAdvice adv bp = 
 BusinessProcess {
  pid = pid bp,
  ptype = ptype bp,
  objects = (objects bp) ++ [obj|obj <- (objects adv), not ((idflow obj) `elem` ["start","end"]) ] ,
--  transitions = map (<+>)  
  transitions = mapMaybe (<+>)  
                 ([(i,j,k) | (i,j,k) <- ((<*>) bp), not (i `matches` (pointcut adv))] ++ 
                  [(i,y,z) | (i,j,k) <- ((<*>) bp), (x,y,z) <- startTransitions adv, i `matches` (pointcut adv)] ++
                  [(x,j,z) | (i,j,k) <- ((<*>) bp), (x,y,z) <- endTransitions adv, i `matches` (pointcut adv)] ++ 
                  [(x,y,z) | (x,y,z) <- ((<*>) adv), (x, y, z) `notElem` ((startTransitions adv) ++ (endTransitions adv))])
}

evaluateBeforeAdvice :: BusinessProcess -> BusinessProcess -> BusinessProcess
evaluateBeforeAdvice adv bp = 
 BusinessProcess {
  pid = pid bp,
  ptype = ptype bp,
  objects = nub ((objects bp) ++ (objects adv)), 
--  transitions = map (<+>)
  transitions = mapMaybe (<+>)  
                 ([(i,j,k) | (i,j,k) <- ((<*>) bp), not (j `matches` (pointcut adv))] ++ 
                  [(i,y,z) | (i,j,k) <- ((<*>) bp), (x,y,z) <- startTransitions adv, j `matches` (pointcut adv)] ++
                  [(x,j,z) | (i,j,k) <- ((<*>) bp), (x,y,z) <- endTransitions adv, j `matches` (pointcut adv)] ++ 
                  [(x,y,z) | (x,y,z) <- ((<*>) adv), (x, y, z) `notElem` ((startTransitions adv) ++ (endTransitions adv))])
}
 
evaluateAroundAdvice :: BusinessProcess -> BusinessProcess -> BusinessProcess
evaluateAroundAdvice adv bp =
 let objAnnot = [ obj | obj <- (objects bp), obj `matches` (pointcut adv)];
     transitionsTemp = 
      ([(i,j,k) | (i,j,k) <- ((<*>) bp), not (i `matches` (pointcut adv)), not (j `matches` (pointcut adv)) ] ++
       [(i,y,z) | (i,j,k) <- ((<*>) bp), (x,y,z) <- startTransitions adv, j `matches` (pointcut adv)] ++
       [(x,j,z) | (i,j,k) <- ((<*>) bp), (x,y,z) <- endTransitions adv, i `matches` (pointcut adv)] ++
       [(x,y,z) | (x,y,z) <- ((<*>) adv), (x, y, z) `notElem` ((startTransitions adv) ++ (endTransitions adv))]);
     objProceed      = [ obj | obj <- (objects adv), obj == Proceed ]
 in case objProceed of
     -- with Proceed
     [objProceed] -> bp {
                         objects = (objects bp) ++ [obj|obj <- (objects adv), not (obj `elem` [Start, End, Proceed]) ],
                         --  transitions = map (<+>)  
                         transitions = mapMaybe (<+>)  			   
                                       ([(i,obj,k) | (i,j,k) <- transitionsTemp, obj <- objAnnot, j == Proceed ] ++
                                        [(obj,j,k) | (i,j,k) <- transitionsTemp, obj <- objAnnot, i == Proceed ] ++
                                        [(i,j,k) | (i,j,k) <- transitionsTemp, i /= Proceed, j /= Proceed ])
                        }
     -- without Proceed
     otherwise -> 
         bp {
             objects = [obj|obj <- (objects bp), obj `notElem` objAnnot] ++ [obj|obj <- (objects adv), not (obj `elem` [Start, End]) ],
             --transitions = map (<+>) [(i,j,k) | (i,j,k) <- transitionsTemp]
             transitions = mapMaybe (<+>) [(i,j,k) | (i,j,k) <- transitionsTemp]
            }

evaluateAdviceQ :: Id -> BusinessProcessModel -> BusinessProcessModel -> BusinessProcessModel
evaluateAdviceQ advId spl product = 
 let advs = [a | a <- (processes spl), (pid a) == advId]
 in case advs of 
      [adv] -> BPM (map (evaluateAdviceQ' adv) (processes product))
      otherwise -> product
 

evaluateAdviceQ' :: BusinessProcess -> BusinessProcess -> BusinessProcess
evaluateAdviceQ' adv bpProduct = 
 let objs = [obj | obj <- (objects bpProduct), (obj `matches` (pointcut adv))]
 in case objs of 
      [] -> bpProduct     
      otherwise -> evaluateAdviceQ'' adv objs bpProduct


evaluateAdviceQ'' :: BusinessProcess -> [FlowObject] -> BusinessProcess -> BusinessProcess
evaluateAdviceQ'' _ [] bpProduct = bpProduct
evaluateAdviceQ'' adv (o:os) bpProduct =
 case ptype adv of 
   BasicProcess -> bpProduct
   (Advice After  _) -> evaluateAdviceQ'' advRen os (evaluateAfterAdviceQ adv bpProduct o)
   (Advice Before _) -> evaluateAdviceQ'' advRen os (evaluateBeforeAdviceQ adv bpProduct o)
   (Advice Around _) -> evaluateAdviceQ'' advRen os (evaluateAroundAdviceQ adv bpProduct o)
   where
   advRen = adv {
                 objects = [rename oa | oa <- objects adv],
                 --transitions = map (<+>) [(rename i, rename j,k)| (i,j,k) <- ((<*>) adv)]
                 transitions = mapMaybe (<+>) [(rename i, rename j,k)| (i,j,k) <- ((<*>) adv)]
                }
   rename obj 
     | (obj `notElem` [Start, End, Proceed]) = obj {id' = (idflow obj) ++ "'"}
     | otherwise = obj

evaluateAfterAdviceQ :: BusinessProcess -> BusinessProcess -> FlowObject -> BusinessProcess
evaluateAfterAdviceQ adv bp objAnn = 
 BusinessProcess {
  pid = pid bp,
  ptype = ptype bp,
  objects = (objects bp) ++ [obj | obj <- (objects adv), obj `notElem` [Start, End]] ,
  --transitions = map (<+>) 
  transitions = mapMaybe (<+>)
                ([(i,j,k) | (i,j,k) <- ((<*>) bp), i /= objAnn] ++ 
                 [(i,y,z) | (i,j,k) <- ((<*>) bp), (x,y,z) <- startTransitions adv, i == objAnn] ++
                 [(x,j,z) | (i,j,k) <- ((<*>) bp), (x,y,z) <- endTransitions adv, i == objAnn] ++ 
                 [(x,y,z) | (x,y,z) <- ((<*>) adv), (x, y, z) `notElem` ((startTransitions adv) ++ (endTransitions adv))])
}

evaluateBeforeAdviceQ :: BusinessProcess -> BusinessProcess -> FlowObject -> BusinessProcess
evaluateBeforeAdviceQ adv bp objAnn = 
 BusinessProcess {
  pid = pid bp,
  ptype = ptype bp,
  objects = (objects bp) ++ [obj | obj <- (objects adv), obj `notElem` [Start, End]] ,
  --transitions = map (<+>)
  transitions = mapMaybe (<+>)
                ([(i,j,k) | (i,j,k) <- ((<*>) bp), j /= objAnn] ++ 
                 [(i,y,z) | (i,j,k) <- ((<*>) bp), (x,y,z) <- startTransitions adv, j == objAnn] ++
                 [(x,j,z) | (i,j,k) <- ((<*>) bp), (x,y,z) <- endTransitions adv, j == objAnn] ++ 
                 [(x,y,z) | (x,y,z) <- ((<*>) adv), (x, y, z) `notElem` ((startTransitions adv) ++ (endTransitions adv))])
}
 
evaluateAroundAdviceQ :: BusinessProcess -> BusinessProcess -> FlowObject -> BusinessProcess
evaluateAroundAdviceQ adv bp objAnn =
 let objsAnnot = [ o | o <- (objects bp), o == objAnn];
     transitionsTemp = ([(i,j,k) | (i,j,k) <- ((<*>) bp), i /= objAnn, j /= objAnn] ++
                        [(i,y,z) | (i,j,k) <- ((<*>) bp), (x,y,z) <- startTransitions adv, j == objAnn] ++
                        [(x,j,z) | (i,j,k) <- ((<*>) bp), (x,y,z) <- endTransitions adv, i == objAnn] ++
                        [(x,y,z) | (x,y,z) <- ((<*>) adv), (x, y, z) `notElem` ((startTransitions adv) ++ (endTransitions adv))]);
     objProceed      = [ obj | obj <- (objects adv), obj == Proceed ]
 in case objProceed of
     -- with Proceed
     [objProceed] -> bp {
                         objects = (objects bp) ++ [obj | obj <- (objects adv), obj `notElem` [Start, End, Proceed]],
                         --transitions = map (<+>) 
                         transitions = mapMaybe (<+>)
                                        ([(i,obj,k) | (i,j,k) <- transitionsTemp, obj <- objsAnnot, j == Proceed ] ++
                                        [(obj,j,k) | (i,j,k) <- transitionsTemp, obj <- objsAnnot, i == Proceed ] ++
                                        [(i,j,k) | (i,j,k) <- transitionsTemp, i /= Proceed, j /= Proceed ])
                        }
     -- without Proceed
     otherwise -> bp {
                       objects = [obj | obj <- (objects bp), obj `notElem` objsAnnot] ++ 
                                 [obj | obj <- (objects adv), obj `notElem` [Start, End] ],
                       --transitions = map (<+>) [(i,j,k) | (i,j,k) <- transitionsTemp]
                       transitions = mapMaybe (<+>) [(i,j,k) | (i,j,k) <- transitionsTemp]
                     }
                   
matches :: FlowObject -> Pointcut -> Bool
matches fo (Pointcut pc) = pc `elem` (annotations fo)
matches fo (Empty) = False

startTransitions :: BusinessProcess -> [(FlowObject, FlowObject, Condition)] 
startTransitions bp = [(i, j, k) | (i, j, k) <- ((<*>) bp), i == Start] 
  
endTransitions :: BusinessProcess -> [(FlowObject, FlowObject, Condition)]
endTransitions bp = [(i, j, k) | (i, j, k) <- ((<*>) bp), j == End] 

-----------------------------------------


