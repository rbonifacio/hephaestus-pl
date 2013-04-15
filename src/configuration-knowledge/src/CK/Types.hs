module CK.Types where 

import FeatureModel.Types
-- import HplProducts.TestTypes

--   The model used to relate feature expressions 
--   to transformations. The configuration knowledge 
--   guides the 'building' process of SPL instances.
type ConfigurationKnowledge a = [ConfigurationItem a]

data ConfigurationItem a = 
 ConfigurationItem {
   expression :: FeatureExpression,   -- ^ if expression holds True for a product configuration...
   transformations :: [a]             -- ^ the list of transformations would be applied.
 } | 
 ConstrainedConfigurationItem { 
   expression :: FeatureExpression,                -- ^ if expression holds True for a product configuration...
   transformations :: [a],       -- ^ the list of transformations would be applied.
   required :: FeatureExpression,     -- ^ required expression for this configuration 
   provided :: FeatureExpression      -- ^ provided expression for this configuration
 }

constrained :: ConfigurationItem a -> Bool 
constrained (ConstrainedConfigurationItem _ _ _ _) = True
constrained _ = False

