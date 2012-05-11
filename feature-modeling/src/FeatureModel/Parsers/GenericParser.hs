module FeatureModel.Parsers.GenericParser (
 parseFeatureModel, 
 parseInstanceModel, 
 FmFormat ( FMPlugin, FMIde, FMGrammar, SXFM ) 
)
where 

import FeatureModel.Types
import qualified BasicTypes as Core

-- modules related to the FMPlugin parser
import FeatureModel.Parsers.FMPlugin.XmlFeatureParser 
import FeatureModel.Parsers.FMPlugin.XmlFeatureModel (xmlFeature2FeatureTree, xml2FeatureConfiguration) 

-- modules related to the FMIde parser
import FeatureModel.Parsers.FMIde.FMIde2FeatureModel
import FeatureModel.Parsers.FMIde.AbsFMIde
import FeatureModel.Parsers.FMIde.SkelFMIde
import FeatureModel.Parsers.FMIde.ErrM
import FeatureModel.Parsers.FMIde.LexFMIde
import FeatureModel.Parsers.FMIde.ParFMIde

-- modules related to the FMGrammar parser
import qualified FeatureModel.Parsers.FMGrammar.Grammar2FeatureModel as GFMG
import qualified FeatureModel.Parsers.FMGrammar.LexFMGrammar as LFMG
import qualified FeatureModel.Parsers.FMGrammar.SkelFMGrammar as SFMG
import qualified FeatureModel.Parsers.FMGrammar.AbsFMGrammar as AFMG
import qualified FeatureModel.Parsers.FMGrammar.ParFMGrammar as PFMG 
import qualified FeatureModel.Parsers.FMGrammar.ErrM as EFMG

-- modules related to the SXFM parser
import  qualified FeatureModel.Parsers.SXFM.ParsecSXFM as ParsecSXFM

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language( haskellStyle )

import Text.XML.HXT.Core
import Text.XML.HXT.RelaxNG

data FmFormat = FMPlugin | FMIde | FMGrammar | SXFM

-- |
-- The top most function for parsing feature models 
-- in different formats. 
-- 
parseFeatureModel (schema, fileName) format = do
 x <- readFile (fileName) 
 case (format) of 
  FMPlugin -> do
    fm <- translateFMPToFm schema fileName
    return fm
   
  FMIde -> do
    let fm = translateFMIdeToFm (pGrammar (myLexer x))
    return fm

  FMGrammar -> do 
    let fm = translateFMGrammarToFm (PFMG.pFMGrammar (PFMG.myLexer x))
    return fm 

  SXFM  -> do
    r <- parseFromFile ParsecSXFM.parseFeatureModel fileName ; 
    case (r) of
      Left err  -> return $ Core.Fail (show err)
      Right f  -> do let fm = f
                     return $ Core.Success fm

-- | 
-- Parse a feature configuration. This parser 
-- is based on the exported instance models from 
-- FMPlugin
--

parseInstanceModel schema fileName = 
 do  
  errs <- checkXMLFile schema fileName
  case errs of 
   [] -> do 
      instanceModel <- parseInstanceModel' fileName
      return $ instanceModel
   
   otherwise -> do
     let errs' = concat $ map show errs
     return $ Core.Fail errs'
  
parseInstanceModel' fileName = 
 do 
   i <- runX ( xunpickleDocument xpFeatureConfiguration [ withValidate yes
							, withTrace 1
 					               	, withRemoveWS yes
 					               	, withPreserveComment yes
 					               	] (Core.createURI fileName) )
   case i of 
    [x] -> do return $ Core.Success (xml2FeatureConfiguration x)
    otherwise -> return $ Core.Fail "Error parsing instance configuration. Try to check it before parsing."
    

translateFMIdeToFm (Ok g)  = Core.Success (grammarToFeatureModel g)
translateFMIdeToFm (Bad s) = Core.Fail s

translateFMGrammarToFm (EFMG.Ok g) = Core.Success (GFMG.grammarToFeatureModel g)  
translateFMGrammarToFm (EFMG.Bad s) = Core.Fail s

translateFMPToFm schema fileName = 
 do
   errs <- checkXMLFile schema fileName
   case errs of 
     [] -> 
         do 
          u <- runX ( xunpickleDocument xpFeature [ withValidate yes
                                                  , withTrace 1
                                                  , withRemoveWS yes
                                                  , withPreserveComment yes
                                                  ] (Core.createURI fileName));
          case u of 
            [x] -> return $ Core.Success (FeatureModel { fmTree = (xmlFeature2FeatureTree x), fmConstraints = [] })
            otherwise -> return $ Core.Fail "Error parsing feature model. Try to check it before parsing."

     -- errors found after checking the FMPlugin file 
     otherwise -> return $ Core.Fail ("Error parsing feature model. " ++ (concat [show e | e <- errs]))

checkXMLFile schema fileName = 
 do 
   errs <- runX ( errorMsgCollect 
                  >>> 
                  readDocument [ withValidate yes
                               , withRelaxNG (Core.createURI schema)
                               , withErrors yes
                               ] (Core.createURI fileName)
                  >>>
                  getErrorMessages
                ) ;
   return errs