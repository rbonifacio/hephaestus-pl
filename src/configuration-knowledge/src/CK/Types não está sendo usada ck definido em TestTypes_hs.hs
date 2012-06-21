module CK.Types where 

import FeatureModel.Types
import HplProducts.TestTypes

--   The model used to relate feature expressions 
--   to transformations. The configuration knowledge 
--   guides the 'building' process of SPL instances.
type ConfigurationKnowledge = [ConfigurationItem]

data ConfigurationItem = 
 ConfigurationItem {
   expression :: FeatureExpression,                -- ^ if expression holds True for a product configuration...
   transformations :: [TransformationModel]        -- ^ the list of transformations would be applied.
 } | 
 ConstrainedConfigurationItem { 
   expression :: FeatureExpression,                -- ^ if expression holds True for a product configuration...
   transformations :: [TransformationModel],       -- ^ the list of transformations would be applied.
   required :: FeatureExpression,     -- ^ required expression for this configuration 
   provided :: FeatureExpression      -- ^ provided expression for this configuration
 }

constrained :: ConfigurationItem -> Bool 
constrained (ConstrainedConfigurationItem _ _ _ _) = True
constrained _ = False

