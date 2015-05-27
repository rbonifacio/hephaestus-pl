module HplAssets.DTMC.PrettyPrinter.DotPP (exportDtmcDot)
where 

import BasicTypes
import HplAssets.DTMC.Types
import Control.Monad
import System.Directory
import System.FilePath

import Data.FDTMC
import Data.FDTMC.Printers.Dot (writeDotFile)


-- | Neste caso, o @path@ é para um diretório
exportDtmcDot:: FilePath -> DtmcModel -> IO()
exportDtmcDot path dtmcModel = do
    createDirectoryIfMissing True path
    let models = dtmcs dtmcModel
    mapM_ (exportOne path) models


exportOne :: FilePath -> Dtmc -> IO()
exportOne path dtmc = writeDotFile (path </> fdtmcId) cleanFDTMC
    where
        cleanFDTMC = pruneUnreachableStates $ chain dtmc
        fdtmcId = dtmcId dtmc
