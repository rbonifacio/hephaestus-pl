module HplAssets.DTMC.PrettyPrinter.DotPP (exportDtmcDot)
where 

import BasicTypes
import HplAssets.DTMC.Types
import Control.Monad
import System.FilePath

import Data.FDTMC
import Data.FDTMC.Printers.Dot (writeDotFile)


-- TODO: FilePath precisa ser um diretório.
exportDtmcDot:: FilePath -> DtmcModel -> IO()
exportDtmcDot f dtmcModel = writeDotFile f $ pruneUnreachableStates fdtmc
    where
        fdtmc = chain (head $ dtmcs dtmcModel)
