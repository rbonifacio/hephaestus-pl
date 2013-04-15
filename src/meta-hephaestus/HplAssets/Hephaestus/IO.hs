--
-- Reusable driver code (build function) for the Hephaestus product line.
--
module HplAssets.Hephaestus.IO where

import HplProducts.Hephaestus
import HplAssets.Hephaestus.Types
import HplAssets.Hephaestus.MetaData
import FeatureModel.Types
import Language.Haskell.Syntax
import Language.Haskell.Parser
import Language.Haskell.Pretty
import Distribution.ModuleName
import CK.Types
--
-- Invoke the build function for the Hephaestus product line.
-- A feature configuration is to be passed as argument.
-- Composition starts from the base product as asset base.
-- The resulting product (module) is stored in the file system.
--
buildHpl :: FeatureConfiguration -> IO ()
buildHpl fc
 = do
      x <- readFile "HplProducts/Base.hs"
      let (ParseOk y) = parseModule x
      let base = (SPLModel noFeatureModel (HephaestusModel [y]))
      let (InstanceModel _ (HephaestusModel [z])) = build noFeatureModel fc ck base
      let (HsModule _ (Module m) _ _ _) = z
      writeFile (toFilePath (fromString m) ++ ".hs") (prettyPrint z)
  where
  ck = map (\(fe, ts) -> ConfigurationItem fe (map HephaestusTransformation ts)) configurationKnowledge
