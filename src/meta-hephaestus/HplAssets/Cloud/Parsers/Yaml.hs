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

import BasicTypes
import HplAssets.Cloud.Types
import HplAssets.Cloud.Parsers.Json

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
