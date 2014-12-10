module HplAssets.DTMC.Parsers.Dot (parseDtmcModel)
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


emptyDtmc :: DtmcModel -> DtmcModel
emptyDtmc dtmcmodel = dtmcmodel { dtmcs = [] }


parseDtmcModel dtmcsSource = do
    allFiles <- getDirectoryContents dtmcsSource
    let files = filter (endswith ".dot") allFiles
    print $ map (combine dtmcsSource) files
    fdtmcs <- mapM (parseDotFile . (combine dtmcsSource)) files
    let dtmcs = map dtmcFromFile $ zip files fdtmcs 
    let dtmcModel = DtmcModel { dtmcs = dtmcs }
    writeDotFile "/tmp/teste.dot" $ resolve (head fdtmcs) ['p']
    return $ Success dtmcModel


dtmcFromFile :: (FilePath, FDTMC) -> Dtmc
dtmcFromFile (filename, fdtmc) = Dtmc { dtmcId = filename,
                                        chain = fdtmc }
