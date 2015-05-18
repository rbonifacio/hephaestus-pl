-----------------------------------------------------------------------------
-- |
-- Module      :  ReqModel.PrettyPrinter.Latex
-- Copyright   :  (c) Rodrigo Bonifacio 2008, 2009
-- License     :  LGPL
--
-- Maintainer  :  rba2@cin.ufpe.br
-- Stability   :  provisional
-- Portability :  portable
--
--
--
-----------------------------------------------------------------------------

module HplAssets.ComponentModel.ExportSourceCode(exportSourceCode) where 

import BasicTypes 
import System.IO
import System.Directory
import System.FilePath
import Control.Exception


import HplAssets.ComponentModel.Types

exportSourceCode :: FilePath -> ComponentModel -> IO ()
exportSourceCode o cm =
 let 
   bd = components cm
   s  = srcDir cm  
 in do 
    copySourceFiles s o bd
--  exportBuildFile  (o ++ "/build.lst") (buildEntries bd)
--  preprocessFiles (o ++ "/build.lst") (preProcessFiles bd) o


copySourceFiles source out [] = return ()
copySourceFiles source out (c:cs) = 
 do 
  createOutDir out c 
  copySourceFile source out c
  copySourceFiles source out cs

-- exportBuildFile f es = 
--  bracket (openFile f WriteMode)
--          hClose
--          (\h -> hPutStr h (concat [e ++ "\n" | e <- es]))

createOutDir out c =
 do
  print $ "Selected output dir: " ++ out
  print $ "Selected output component" ++ (snd c)
  let new = out </> (snd c)
  let newDir = dropFileName new
  print new 
  print newDir 
  createDirectoryIfMissing True newDir

-- copy a component from source directory to the out directory. 
-- Note that a component is a pair of ids (Id, Id), indicating the 
-- names of the input and output files. Usually they have the same 
-- name. 
copySourceFile source out c = 
 do 
  let old = source </> (fst c)
  let new = out </> (snd c)
  print ("Copying file " ++ old ++ " to " ++ new)
  copyFile old new

