module HplAssets.DTMC.Parsers.Dot (parseDtmcModel)
where 

import BasicTypes

import HplAssets.DTMC.Types

import Data.FDTMC (FDTMC)
import Data.FDTMC.Parsers.Dot (parseDotFile)

import Control.Monad
import Data.String.Utils (endswith)
import System.Directory
import System.IO.Unsafe


emptyDtmc :: DtmcModel -> DtmcModel
emptyDtmc dtmcmodel = dtmcmodel { dtmcs = [] }


parseDtmcModel dtmcsSource = do
    allFiles <- getDirectoryContents dtmcsSource
    let files = filter (endswith ".dot") allFiles
    fdtmcs <- mapM parseDotFile files
    let dtmcs = map dtmcFromFile $ zip files fdtmcs 
    return $ Success $ DtmcModel { dtmcs = dtmcs }


dtmcFromFile :: (FilePath, FDTMC) -> Dtmc
dtmcFromFile (filename, fdtmc) = Dtmc { dtmcId = filename,
                                        chain = fdtmc }
