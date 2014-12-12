module HplAssets.DTMC.PrettyPrinter.DotPP (exportDtmcDot)
where 

import BasicTypes

import HplAssets.DTMC.Types

import Data.FDTMC (FDTMC, resolve)
import Data.FDTMC.Parsers.Dot (parseDotFile)
import Data.FDTMC.Printers.Dot (writeDotFile)

import Control.Monad
import Data.String.Utils (endswith)
import System.Directory
import System.FilePath



exportDtmcDot:: FilePath -> DtmcModel -> IO()
exportDtmcDot f dtmc = do
    writeDotFile "/tmp/teste.dot" $ resolve dtmc

