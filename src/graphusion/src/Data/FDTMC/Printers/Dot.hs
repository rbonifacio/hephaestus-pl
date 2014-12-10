module Data.FDTMC.Printers.Dot where


import Data.FDTMC

import Data.GraphViz (graphToDot,
                      GraphvizParams(..),
                      NodeCluster(..))
import Data.GraphViz.Attributes.Complete (Attributes,
                                          Attribute (..),
                                          Label (StrLabel))
import qualified Data.GraphViz.Commands.IO as GraphViz
import Data.GraphViz.Types (GraphID (Num),
                            Number (Int))
import Data.GraphViz.Types.Canonical (DotGraph)

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree (Gr)  -- Instância de Graph

import qualified Data.Text.Lazy as Text (pack)


writeDotFile :: FilePath -> FDTMC -> IO ()
writeDotFile path fdtmc = GraphViz.writeDotFile path $  fdtmcToDot fdtmc


fdtmcToDot :: FDTMC -> DotGraph Node
fdtmcToDot = fglToDot . fdtmcToFGL


fdtmcToFGL :: FDTMC -> Gr String String
fdtmcToFGL = (nmap stateToString) . (emap transitionToString)


fglToDot :: Gr String String -> DotGraph Node
fglToDot = graphToDot params
    where
        params :: GraphvizParams Node String String () String
        params = Params { isDirected       = True
                        , globalAttributes = []
                        , clusterBy        = N
                        , isDotCluster     = const True
                        , clusterID        = const (Num $ Int 0)
                        , fmtCluster       = const []
                        , fmtNode          = \(_, label) -> [labelFromString label]
                        , fmtEdge          = \(_, _, label) -> [labelFromString label]
                        }


labelFromString :: String -> Attribute
labelFromString = (Label . StrLabel . Text.pack)
