module HplAssets.UCM.PrettyPrinter.XML (exportUcmToXML) where

import BasicTypes

import IO

import Text.PrettyPrint.HughesPJ
import HplAssets.UCM.Types


exportUcmToXML:: FilePath -> UseCaseModel -> IO()
exportUcmToXML f ucm = 
  --do
  --print $ f
  --print $ show ucm
 bracket (openFile f WriteMode)
         hClose
         (\h -> hPutStr h (show (ucmToXML ucm)))


ucmToXML :: UseCaseModel -> Doc
ucmToXML (UCM name ucs as)  =  vcat [ text "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>" 
                                    , text "<phone xmlns=\"user-view.target.v20071129\">"
                                    , text "<feature>"
                                    , text "<featureId>01</featureId> "
                                    , text ("<name>" ++ (xmlData name) ++ "</name>")
                                    , ucsToXML ucs 
                                    , text ("</feature>")
                                    , text "</phone>"
                                    ]
ucsToXML :: [UseCase] -> Doc
ucsToXML ucs = vcat $ map ucToXML ucs 

ucToXML :: UseCase -> Doc
ucToXML (UseCase i n d s) = vcat [ text "<useCase>"
                                 , text ("<id>" ++ i ++ "</id>")
                                 , text ("<name>" ++ (xmlData n) ++ "</name>")
                                 , text ("<description>" ++ (xmlData d) ++ "</description>") 
                                 , text ("<setup></setup>")
		            	 , scenariosToXML s
                                 , text "</useCase>"
                                 ]




scenariosToXML :: [Scenario] -> Doc
scenariosToXML scs = vcat $ map scenarioToXML scs

scenarioToXML :: Scenario -> Doc
scenarioToXML (Scenario i d f s t) = vcat [ text "<flow>"
                                          -- , text ("<id>" ++ i ++ "</id>") -- TODO: This version, TaRGeT does not support scenario ids.
                                          , text ("<description>" ++ (xmlData d) ++ "</description>") 
		                          , hcat (text "<fromSteps>" : ((punctuate (text "," ) (map idRefToXML f)) ++ [(text "</fromSteps>")]) )
                                          , hcat (text "<toSteps>" : ((punctuate (text "," ) (map idRefToXML t)) ++ [(text "</toSteps>")]) )
			                  , stepsToXML s
                                          , text "</flow>"
                                          ]

stepsToXML :: [Step] -> Doc
stepsToXML steps = vcat $ map stepToXML steps

stepToXML :: Step -> Doc
stepToXML  (Step sid a s r an) = vcat [ text "<step>" 
                                   , text ("<stepId>" ++ sid ++ "</stepId>")
                                   , text ("<action>" ++ (xmlData a) ++ "</action>")
                                   , text ("<condition>"++ (xmlData s) ++ "</condition>") 
				   , text ("<response>" ++ (xmlData r) ++ "</response>")
				   , text "</step>"
                                   ]


idRefToXML :: StepRef -> Doc
idRefToXML (IdRef s) = text s
idRefToXML (AnnotationRef s) = text ('@':s)  


xmlData :: String -> String 
xmlData s = xmlData' scs s 
 where
  xmlData' [] s = s
  xmlData' (c:cs) s = xmlData' cs (replaceString (fst c) (snd c) s)
  scs = [ ("<", "\&lt")
        , ("<", "\&gt")
        ]



