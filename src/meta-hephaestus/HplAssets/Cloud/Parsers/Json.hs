{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Cloud.Parsers.Json
-- Copyright   :  (c) authors and contributors, 2015
-- License     :  LGPL
--
-- Stability   :  provisional
-- Portability :  portable
--
-- A JSON parser for our cloud model.
--
-----------------------------------------------------------------------------
module HplAssets.Cloud.Parsers.Json (parseJSONCloudModel)
where

import Data.Aeson
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B

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

jsonFile :: FilePath
jsonFile = ""
--
parseJSONCloudModel  :: IO B.ByteString
parseJSONCloudModel = B.readFile jsonFile
