module Data.FDTMC (
    FDTMC,
    FeatureSelection,
    Feature,
    fromStringGraph,
    toStringGraph,
    resolve,
    pruneUnreachableStates
) where

import Data.Graph.Inductive.Basic (elfilter)
import Data.Graph.Inductive.Graph (
        Context, DynGraph, Edge, Graph, Node,
        (&), delNodes, edges, emap, indeg, inn, newNodes, nmap, nodes, suc
    )
import Data.Graph.Inductive.PatriciaTree (Gr)  -- Instância de Graph
import Data.Graph.Inductive.Query.MaxFlow (maxFlowgraph)
import Data.Logic.Propositional (Expr,
                                 Mapping,
                                 interpret,
                                 parseExpr,
                                 Var (..))

import Data.Either (either)
import Data.List (null)
import Data.Map (fromList)
import Data.String.Utils (replace)
import Text.Printf (printf)


-- | DTMC com anotação de variabilidade (probabilidades de transição
-- anotadas por feature expressions).
type FDTMC = Gr StateNode Transition
-- | Representa o label e as anotações do nó. Por enquanto, trata tudo
-- como uma string.
type StateNode = String
-- | Transição numa FDTMC. Pode ser uma feature expression ou uma
-- simples probabilidade de transição.
data Transition = FeatureExpression Expr | Probability Float
    deriving (Show)

type FeatureSelection = [Feature]
type Feature = Char


fromStringGraph :: Gr String String -> FDTMC
fromStringGraph = (nmap stateFromString) . (emap transitionFromString)


toStringGraph :: FDTMC -> Gr String String
toStringGraph = (nmap stateToString) . (emap transitionToString)


-- | Converte um estado da FDTMC para string.
-- Elimina anotações e metadados.
stateToString :: StateNode -> String
stateToString = id  -- TODO: remover anotações


-- | Converte uma transição da FDTMC para string.
transitionToString :: Transition -> String
transitionToString (Probability p) = printf "%f" p -- sem isso usa o formato científico...
transitionToString (FeatureExpression e) = show e


stateFromString :: String -> StateNode
stateFromString = id


transitionFromString :: String -> Transition
transitionFromString label = either parseProbability FeatureExpression expr
    where
        expr = parseExpr "" label
        parseProbability _ = Probability probability
        probability = read (replace "," "." label) :: Float


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
