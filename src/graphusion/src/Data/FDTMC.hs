module Data.FDTMC (
    FDTMC,
    FeatureSelection,
    Feature,
    fromStringGraph,
    toStringGraph,
    resolve,
    pruneUnreachableStates,
    append,
    compose
) where

import Data.Graph.Inductive.Basic (elfilter)
import Data.Graph.Inductive.Graph (
        Context, DynGraph, Edge, Graph, Node, LNode,
        (&), delEdge, delEdges, delNodes, edges, emap, indeg, inn, insEdge, insEdges, insNodes,
        labEdges, labNodes, lsuc, mkGraph, newNodes, nmap, nodeRange, nodes, out, suc
    )
import Data.Graph.Inductive.PatriciaTree (Gr)  -- Instância de Graph
import Data.Graph.Inductive.Query.MaxFlow (maxFlowgraph)
import Data.Logic.Propositional (Expr,
                                 Mapping,
                                 interpret,
                                 parseExpr,
                                 Var (..))

import Data.Either (either)
import Data.Map (fromList)
import Data.String.Utils (join, replace, split)
import Text.Printf (printf)


-- | DTMC com anotação de variabilidade (probabilidades de transição
-- anotadas por feature expressions).
type FDTMC = Gr StateNode Transition
-- | Representa o label e as anotações do nó. Por enquanto, trata tudo
-- como uma string.
data StateNode = StateNode { label :: String
                           , annotations :: [Annotation] }
    deriving (Show)
type Annotation = String
type Pointcut = String

-- | Transição numa FDTMC. Pode ser uma feature expression ou uma
-- simples probabilidade de transição.
data Transition = FeatureExpression Expr | Probability Float
    deriving (Show)

type FeatureSelection = [Feature]
type Feature = Char


fromStringGraph :: Gr String String -> FDTMC
fromStringGraph = (nmap stateFromString) . (emap transitionFromString)


toStringGraph :: FDTMC -> Gr String String
toStringGraph = (nmap $ stateToString . removeAnnotations) . (emap transitionToString)


-- | Converte um estado da FDTMC para string.
stateToString :: StateNode -> String
stateToString state = join "\n" $ (label state) : map ("@" ++) (annotations state)


-- | Converte uma transição da FDTMC para string.
transitionToString :: Transition -> String
transitionToString (Probability p) = printf "%f" p -- sem isso usa o formato científico...
transitionToString (FeatureExpression e) = show e


stateFromString :: String -> StateNode
stateFromString state = StateNode { label = label
                                  , annotations = annotations }
    where
        label:annotations = split "@" state


transitionFromString :: String -> Transition
transitionFromString label = either parseProbability FeatureExpression expr
    where
        expr = parseExpr "" label
        parseProbability _ = Probability probability
        probability = read (replace "," "." label) :: Float


removeAnnotations :: StateNode -> StateNode
removeAnnotations stateNode = stateNode { annotations = [] }


-- | Resolve as variabilidades de um FDTMC com base numa seleção de features.
-- Aproveitando o ensejo, remove os estados inalcançáveis.
resolve :: FDTMC -> FeatureSelection -> FDTMC
resolve fdtmc features = emap resolveTransition fdtmc
    where
        resolveTransition transition = case transition of
            (FeatureExpression expr) -> Probability $ interpret' expr truthValues
            otherwise -> transition
        truthValues = fromList [(Var feature, True) | feature <- features]


interpret' :: Expr -> Mapping -> Float
interpret' expr truthValues = if interpret expr truthValues then 1.0 else 0.0


-- | Remove os estados inalcançáveis do FDTMC
pruneUnreachableStates :: FDTMC -> FDTMC
pruneUnreachableStates = (emap floatToTransition)
                         . pruneZeroTransitions
                         . pruneIsolatedNodes
                         . (emap transitionToFloat)

floatToTransition :: Float -> Transition
floatToTransition = Probability


transitionToFloat :: Transition -> Float
transitionToFloat (Probability p) = p
transitionToFloat (FeatureExpression e) = 0.0  -- TODO: Ver o que fazer se encontrar uma FeatureExpression


pruneIsolatedNodes :: (DynGraph gr, Num edge, Ord edge) => gr node edge -> gr node edge
pruneIsolatedNodes graph = delNodes (isolatedNodes graph) graph

isolatedNodes :: (DynGraph gr, Num edge, Ord edge) => gr node edge -> [Node]
isolatedNodes graph = filter (not . hasFlow) $ nodes graph
    where
        hasFlow node = (node == startNode graph) || (any hasFlow' $ inEdges node)
        inEdges = inn graphWithFlow
        graphWithFlow = totalFlow graph
        hasFlow' (_, _, (flow, capacity)) = flow /= 0


pruneZeroTransitions :: (DynGraph gr, Num edge, Ord edge) => gr node edge -> gr node edge
pruneZeroTransitions graph = elfilter (/= 0) graph


totalFlow :: (DynGraph gr, Num edge, Ord edge) => gr node edge -> gr () (edge, edge)
totalFlow graph = maxFlowgraph confluentGraph startNode' finalNode
    where
        confluentGraph = confluent graph
        startNode' = startNode confluentGraph
        finalNode = head $ finalNodes confluentGraph


-- | Constrói uma versão confluente do grafo original, ou seja, um grafo
-- com um nó a mais e, para cada nó final (sem arestas de saída), uma
-- aresta com peso 1 apontando para o novo nó ("sink").
-- Desta forma, o retorno é um grafo `graph` que satisfaz o predicado:
--
--      length (finalNodes graph) == 1
--
-- Esta construção é útil porque uma FDTMC pode ter vários estados finais,
-- e para descobrir quais são inatingíveis precisamos simular um fluxo que
-- passe por todo o grafo.
confluent :: (DynGraph gr, Num edge, Ord edge) => gr node edge -> gr () edge
confluent graph = (sinkNodeContext graph) & (unlabeledNodes graph)
    where
        unlabeledNodes = nmap (const ())


sinkNodeContext :: (DynGraph gr, Num edge, Ord edge) => gr node edge -> Context () edge
sinkNodeContext graph = (linksToSinkNode, sinkNode, (), [])
    where
        sinkNode = head $ newNodes 1 graph
        linksToSinkNode = [(1, finalNode) | finalNode <- finalNodes graph]


startNode :: (DynGraph gr) => gr node edge -> Node
startNode graph = head $ filter noPredecessors $ nodes graph
    where
        noPredecessors node = indeg graph node == 0


finalNodes :: (DynGraph gr, Num edge, Ord edge) => gr node edge -> [Node]
finalNodes graph = filter isFinal $ nodes graph
    where
        isFinal node = case suc graph node of
            [] -> True
            [x] -> x == node
            otherwise -> False


-- | Insere uma FDTMC isomórfica a @fragment@ na FDTMC @base@ nos nós
-- selecionados com o @pointcut@.
-- Pré-condições:
--
--      * @base[pointcut]@ deve ser um estado final.
--
append :: FDTMC -> FDTMC -> Pointcut -> FDTMC
append base fragment pointcut = append' base fragment joinpoints
    where
        joinpoints = evaluatePointcut base pointcut


append' :: FDTMC -> FDTMC -> [Node] -> FDTMC
append' base fragment [] = base
append' base fragment (jp:jps) = append' partial fragment jps
    where
        partial = appendAt base fragment jp


appendAt :: FDTMC -> FDTMC -> Node -> FDTMC
appendAt base fragment node = linkToFragStart . delLoop $ base `union` fragment'
    where
        linkToFragStart = insEdge (node, startNode fragment', Probability 1.0)
        delLoop graph = delEdges outEdges graph
        outEdges = map (\(s, e, _) -> (s, e)) $ out base node
        fragment' = fragment `renameWithRespectTo` base


evaluatePointcut :: FDTMC -> Pointcut -> [Node]
evaluatePointcut fdtmc pointcut = map fst . filter (`matches` pointcut) $ labNodes fdtmc
    where
        matches (_, state) pointcut = pointcut `elem` (annotations state)


-- | Faz a união dos conjuntos de arestas e de nós de duas FDTMC, mas sem
-- adicionar arestas de ligação de um para outro.
-- Assume que a interseção entre os conjuntos de nós dos grafos é vazia.
union :: FDTMC -> FDTMC -> FDTMC
union f1 f2 = insEdges (labEdges f2) $ insNodes (labNodes f2) f1


renameWithRespectTo toRename base = shiftNodesBy (maxBaseNode + 1) toRename
    where
        maxBaseNode = snd . nodeRange $ base


shiftNodesBy :: Node -> FDTMC -> FDTMC
shiftNodesBy amount fdtmc = mkGraph shiftedNodes shiftedEdges
    where
        shiftedNodes = [(n + amount, state) | (n, state) <- labNodes fdtmc]
        shiftedEdges = [(from + amount, to + amount, transition) | (from, to, transition) <- labEdges fdtmc]


-- | Insere a FDTMC `fragment` na FDTMC `base` no lugar da aresta entre os
-- nós base[startPointcut] e base[endPointcut].
-- Pré-condições:
--
--      * deve haver uma aresta @base[startPointcut]@ -> @base[endPointcut]@,
--          que será substituída pela FDTMC @fragment@ com a mesma
--          probabilidade de transição.
--      * @fragment@ deve possuir __exatamente__ um estado final
--          não-absorvente (sem arestas de saída).
--
compose :: FDTMC -> FDTMC -> Pointcut -> Pointcut -> FDTMC
compose base fragment startPointcut endPointcut = compose' base fragment joinpoints
    where
        startJointpoints = evaluatePointcut base startPointcut
        endJointpoints = evaluatePointcut base endPointcut
        joinpoints = [(s, e) | s <- startJointpoints, e <- endJointpoints, e `elem` (suc base s)]


compose' :: FDTMC -> FDTMC -> [(Node , Node)] -> FDTMC
compose' base fragment [] = base
compose' base fragment ((start, end):joinpoints) = compose' partial fragment joinpoints
    where
        partial = composeAt base fragment start end


composeAt :: FDTMC -> FDTMC -> Node -> Node -> FDTMC
composeAt base fragment start end = linkToFragStart . linkFromFragEnd $ base `union` fragment'
    where
        fragment' = fragment `renameWithRespectTo` base
        linkToFragStart fdtmc = redirectEdge fdtmc start end (startNode fragment')
        linkFromFragEnd = insEdge (strictlyFinalNode fragment', end, Probability 1.0)


redirectEdge :: (DynGraph gr) => gr node edge -> Node -> Node -> Node -> gr node edge
redirectEdge graph from currentTo newTo = insEdge newEdge $ delEdge currentEdge $ graph
    where
        newEdge = (from, newTo, label)
        currentEdge = (from, currentTo)
        label = if (length candidates == 1)
                    then snd $ head candidates
                    else error "Existe mais de uma aresta entre dois nós"
        candidates = filter ((== currentTo) . fst) $ lsuc graph from


strictlyFinalNode :: (DynGraph gr) => gr node edge -> Node
strictlyFinalNode graph = if length finals == 1
                            then head finals
                            else error "O fragmento deve ter exatamente um estado final"
    where
        finals = filter (null . suc graph) $ nodes graph
