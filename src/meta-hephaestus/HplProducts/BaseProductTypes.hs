-- 
-- We include instructions for customizing the Types module of the base product.
--

--{-# OPTIONS -fglasgow-exts #-}
module HplProducts.BaseProductTypes where

-- Add imports Types module of the product-specific assets
import FeatureModel.Types hiding (Success,Fail)
import Data.Generics
import BasicTypes

data SPLModel = SPLModel {
  featureModel :: FeatureModel
  -- Add product-specific model parts
}

data InstanceModel = InstanceModel {
  featureConfiguration :: FeatureConfiguration
  -- Add product-specific model parts
} deriving (Data, Typeable)


-- *******************************************************************************************************************************
-- we move the TransformationModel data type from BaseProduct.hs module to BaseProductTypes.hs module to execute the CK XML parser code
-- we bring the CK definition from Hephaestus code e remove it of BaseProduct.hs module, too.
-- *******************************************************************************************************************************

-- Add embedding constructors for product-specific transformations
data TransformationModel = UndefinedTransformation

-- Add embedding constructors for product-specific exports. Used by function export of BaseProduct.hs
data ExportModel = UndefinedExport

-- Add embedding constructors for product-specific exports. Used by command sequence_ in function main() of BaseProduct.hs
lstExport::[ExportModel]
lstExport = []


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


-----------------------------------------------------------------------------------------
-- This function is used by CK ParserXML. It was brought from module CK.Parsers.XML.XmlConfigurationKnowledge of Hephaestus 
-----------------------------------------------------------------------------------------       
xml2Transformation :: String -> [String] -> ParserResult TransformationModel
xml2Transformation "Undefined" _ = undefined

