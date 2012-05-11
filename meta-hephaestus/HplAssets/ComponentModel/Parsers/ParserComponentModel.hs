module HplAssets.ComponentModel.Parsers.ParserComponentModel (parseComponentModel)
where 

import BasicTypes

import HplAssets.ComponentModel.Parsers.AbsComponentModel
import HplAssets.ComponentModel.Parsers.SkelComponentModel
import HplAssets.ComponentModel.Parsers.ErrM
import HplAssets.ComponentModel.Parsers.LexComponentModel
import HplAssets.ComponentModel.Parsers.ParComponentModel

import qualified HplAssets.ComponentModel.Types as T

parseComponentModel fileName = do 
 x <- readFile (fileName) 
 let cm = parseResult (pComponentModel (myLexer x))
 return cm

parseResult (Ok g)  = Success (translateModel g)
parseResult (Bad s) = Fail s

translateModel :: ComponentModel -> T.ComponentModel
translateModel (TComponentModel cs) = map translateMapping cs 

translateMapping :: ComponentMapping -> T.ComponentMapping 
translateMapping (TComponentMapping (Ident i) p) = (i, path p)

path :: RelativePath -> String
path (BasicFilePath (Ident n)) = n
path (BasicFileExt (Ident n)) = "." ++ n
path (BasicFilePathExt (Ident n) (Ident e)) = n ++ "." ++ e
path (ComposedFilePath (Ident i) p ) = i ++ "/" ++ path p