{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Cloud.Parsers.Yaml
-- Copyright   :  (c) authors and contributors, 2015
-- License     :  LGPL
--
-- Stability   :  provisional
-- Portability :  portable
--
-- A YAML parser for our cloud model.
--
-----------------------------------------------------------------------------
module HplAssets.Cloud.Parsers.Yaml (parseCloudModel)
where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Y

import Data.Aeson
import Control.Applicative
import Control.Monad

import BasicTypes
import HplAssets.Cloud.Types

-- Instances to convert our type from JSON

instance FromJSON CloudModel where
  parseJSON (Object m) = 
     CloudModel <$> m .: "clouds"      
  parseJSON _ = mzero

instance FromJSON Cloud where
  parseJSON (Object m) =
     Cloud <$> m .: "cloud-id"
           <*> m .: "name"
           <*> m .: "instances"  

instance FromJSON Instance where
  parseJSON (Object m) = 
     Instance <$> m .: "instance-id"
              <*> m .: "instance-name"           
              <*> m .: "location"
              <*> m .: "security-group"
              <*> m .: "image-id"
              <*> m .: "access-key"
  parseJSON _ = mzero
   
--instance FromJSON Location where
--  parseJSON (Object m) = 
--     Location <$> m .: "region"
--              <*> m .: "zone"
--   parseJSON _ = mzero

-- This is an elegant way to serialize Generic type as JSON. However, it can only be used
-- iff the there isn't any customization on the attributes's name. It is important to highlight
-- that the type to serialize must be Generic.
instance FromJSON Location



yamlFile :: FilePath
yamlFile = "cloud-model.yaml"

parseCloudModel yamlFile = 
 do 
   content <- BS.readFile yamlFile
   let parsedModel = Y.decode content :: Maybe CloudModel
   case parsedModel of     
     Nothing -> return $ Fail "Could not parse the YAML cloud model. Please check the input file."
--     (Just (CloudModel clouds)) -> return $ Success clouds -- only the clouds
     Just c -> return $ BasicTypes.Success c
