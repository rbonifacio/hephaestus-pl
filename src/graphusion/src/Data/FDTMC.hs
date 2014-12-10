module Data.FDTMC (
    FDTMC,
    FeatureSelection,
    Feature,
    -- Não está legal expor métodos para construir e imprimir estados e
    -- transições... seria mais interessante expor os conversores de FDTMC
    -- para Gr String String e vice versa.
    stateFromString,
    stateToString,
    transitionFromString,
    transitionToString,
    resolve
) where

import Data.Graph.Inductive.Graph (emap)
import Data.Graph.Inductive.PatriciaTree (Gr)  -- Instância de Graph
import Data.Logic.Propositional (Expr,
                                 Mapping,
                                 interpret,
                                 parseExpr,
                                 Var (..))

import Data.Either (either)
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
resolve fdtmc features = pruneUnreachableStates $ emap resolveTransition fdtmc
    where
        resolveTransition transition = case transition of
            (FeatureExpression expr) -> Probability $ interpret' expr truthValues
            otherwise -> transition
        truthValues = fromList [(Var feature, True) | feature <- features]


interpret' :: Expr -> Mapping -> Float
interpret' expr truthValues = if interpret expr truthValues then 1.0 else 0.0


-- | Remove os estados inalcançáveis do FDTMC
pruneUnreachableStates :: FDTMC -> FDTMC
pruneUnreachableStates = id     -- TODO: Implementar a remoção de nós inalcançáveis
