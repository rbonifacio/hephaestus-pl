-----------------------------------------------------------------------------
-- |
-- Module      :  HplAssets.Cloud.PrettyPrinter.DeploymentFile
-- Copyright   :  (c) authors and contributors 2015
-- License     :  LGPL
--
-- Stability   :  provisional
-- Portability :  portable
--
-- A bash script pretty printer for the cloud model.
--
-----------------------------------------------------------------------------

module HplAssets.Cloud.PrettyPrinter.DeploymentFile (exportCloudDeployment)
where

import System.IO
import System.Directory
import System.FilePath

import Control.Exception
import Control.Monad

import Text.PrettyPrint.HughesPJ

import BasicTypes
import HplAssets.Cloud.Types

exportCloudDeployment:: FilePath -> CloudModel -> IO()

exportCloudDeployment path cloudModel = do
  createDirectoryIfMissing True path    
  let cs = clouds cloudModel
  mapM_ (writeOne path) cs
  
writeOne :: FilePath -> Cloud -> IO()
writeOne path cloud = writeCloudDeploymentFile (path </> n) cloud where
  n = name cloud

writeCloudDeploymentFile :: FilePath -> Cloud -> IO ()
writeCloudDeploymentFile file cloud = do 
  let inst = instances cloud  
  mapM_ (writeCloudInstances file) inst

writeCloudInstances :: FilePath -> Instance -> IO ()
writeCloudInstances file instances =
  withFile file WriteMode $ \h ->   
    putStrLn (show (instanceToString instances)) 

instanceToString :: Instance -> String
instanceToString ins =
   "deploy-instance "   ++
   "--instance-id "     ++  instanceId ins        ++ 
   " --image-id "       ++  imageId ins           ++ 
   " --security-group " ++  securityGroup ins     ++ 
   "--key-name "        ++  accessKey ins         ++ 
   "--zone "            ++  (zone $location ins)  ++
   "--region "          ++  (region $location ins)