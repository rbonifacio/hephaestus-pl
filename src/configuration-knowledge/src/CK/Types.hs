{-# LANGUAGE MultiParamTypeClasses #-}

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

class SPL a b where
  makeEmptyInstance :: FeatureConfiguration -> a -> b

-- Provisory. I guess that it will be 
-- possible to remove this type class in a near  
-- future. 
class Product p where
  features :: p -> FeatureConfiguration
  
class (SPL a b, Product b) => Transformation t a b where 
  applyT ::  t -> a -> b -> b
  
build :: (Transformation a b c, SPL b c, Product c) => FeatureModel -> FeatureConfiguration -> ConfigurationKnowledge a -> b -> c
build fm fc ck spl = stepRefinement ts spl emptyInstance       
 where 
  emptyInstance = makeEmptyInstance fc spl
  ts = validTransformations ck fc 

validTransformations :: ConfigurationKnowledge a -> FeatureConfiguration -> [a]
validTransformations ck fc = concat [transformations c | c <-ck, eval fc (expression c)]

stepRefinement :: (Transformation t a b, Product b) => [t] -> a -> b -> b
stepRefinement [] _ p = p
stepRefinement (t:ts) s p = stepRefinement ts s (applyT t s p)
   
constrained :: ConfigurationItem a -> Bool 
constrained (ConstrainedConfigurationItem _ _ _ _) = True
constrained _ = False

