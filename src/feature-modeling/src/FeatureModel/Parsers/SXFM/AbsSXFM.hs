

module FeatureModel.Parsers.SXFM.AbsSXFM where

-- Haskell module generated by the BNF converter




newtype Ident = Ident String deriving (Eq,Ord,Show,Read)
data SxFM =
   TSxFM SxFeatureRoot
  deriving (Eq,Ord,Show,Read)

data SxFeatureRoot =
   TSxFeatureRoot SxFeatureName SxFeatureId [SxFeature]
  deriving (Eq,Ord,Show,Read)

data SxFeatureName =
   TSxFeatureName Ident
  deriving (Eq,Ord,Show,Read)

data SxFeatureId =
   TSxFeatureId Ident
  deriving (Eq,Ord,Show,Read)

data SxFeature =
   TSxMandatoryFeature SxFeatureName [SxFeature]
 | TSxOptionalFeature SxFeatureName [SxFeature]
 | TSxAlternativeFeature [SxOption]
 | TSxOrFeature [SxOption]
  deriving (Eq,Ord,Show,Read)

data SxOption =
   TSxOption Ident Ident [SxFeature]
  deriving (Eq,Ord,Show,Read)
