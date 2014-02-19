module HplAssets.BPM.PrettyPrinter.XML where

import BasicTypes

import Control.Exception
import System.IO

import Text.PrettyPrint.HughesPJ
import HplAssets.BPM.Types

exportBpmToXML:: FilePath -> BusinessProcessModel -> IO()
exportBpmToXML f bpm = 
 bracket (openFile f WriteMode)
         hClose
         (\h -> hPutStr h (show (bpmToXML bpm)))

bpmToXML :: BusinessProcessModel -> Doc
bpmToXML bp = vcat [ text "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>" 
                                    , text "<phone xmlns=\"user-view.target.v20071129\">"
                                    , text "<feature>"
                                    , text "<featureId>01</featureId> "
                                    , text "(<name> ++ (xmlData name) ++ </name>)"
                                    , text "bpsToXML bps" 
                                    , text ("</feature>")
                                    , text "</phone>"
                                    ]
