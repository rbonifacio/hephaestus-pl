module HplAssets.Hephaestus.Parser.HephaestusParser where

import HplAssets.Hephaestus.Types
import BasicTypes
     
-- imports to function parserHephaestus
import Language.Haskell.Parser

-- import to function outputHephaestus
import Language.Haskell.Syntax
import Language.Haskell.Pretty
import Distribution.ModuleName

parserHephaestus =
  do
      x <- readFile "HplProducts/BaseProduct.hs"
      let (ParseOk y) = parseModule x
      x2 <- readFile "HplProducts/BaseProductTypes.hs"
      let (ParseOk y2) = parseModule x2
      let hephModel = HephaestusModel [y, y2]
      return $ Success hephModel
      
      
outputHephaestus:: HephaestusModel -> IO()
outputHephaestus (HephaestusModel [p1, p2]) = do
      let (HsModule _ (Module m) _ _ _) = p1
      writeFile (toFilePath (fromString m) ++ ".hs") (prettyPrint p1)
      let (HsModule _ (Module m) _ _ _) = p2
      writeFile (toFilePath (fromString m) ++ ".hs") (prettyPrint p2)
      return ()