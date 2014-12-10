module Data.FDTMC.Parsers.Dot where

import Data.FDTMC

import Data.GraphViz (dotToGraph)
import Data.GraphViz.Attributes.Complete (Attributes,
                                          Attribute (..),
                                          Label (StrLabel),
                                          sameAttribute)
import Data.GraphViz.Commands.IO
import Data.GraphViz.Types.Generalised

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree (Gr)  -- Instância de Graph

import Data.List (find)
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as Text (empty,
                                         unpack)


parseDotFile :: FilePath -> IO (FDTMC)
parseDotFile path = fmap dotToFDTMC (readDotFile path)


dotToFDTMC = fglToFDTMC . dotToFGL


fglToFDTMC :: Gr String String -> FDTMC
fglToFDTMC = (nmap parseState) . (emap parseTransition)
    where
        parseState = stateFromString
        parseTransition = transitionFromString


dotToFGL :: DotGraph Node -> Gr String String
dotToFGL = (nmap attrsToString) . (emap attrsToString) . dotToGraph


attrsToString :: Attributes -> String
attrsToString attrs = fromMaybe "" $ fmap labelToString (find isLabel attrs)


labelToString :: Attribute -> String
labelToString (Label (StrLabel text)) = Text.unpack text
labelToString _ = ""


isLabel :: Attribute -> Bool
isLabel = sameAttribute (Label (StrLabel Text.empty))
