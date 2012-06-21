{-# OPTIONS -fglasgow-exts #-}
module HplAssets.Hephaestus.Types where

import Language.Haskell.Syntax
import Data.Generics

data HephaestusTransformation
   = SelectBaseProduct
   | SelectAsset String
   | SelectExport String
   | BindProductName String  
   | RemoveProductMainFunction
   | SelectCKParser
 deriving (Show)

data HephaestusModel = HephaestusModel [HsModule] deriving (Show, Data, Typeable) 

data ExportHephaestusDoc = ExportDoc

