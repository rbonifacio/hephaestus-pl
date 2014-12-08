module HplAssets.DTMC.Parsers.Dot (parseDtmcModel,dtmcSchema)
where 

import BasicTypes

import HplAssets.DTMC.Types

emptyDtmc :: DtmcModel -> DtmcModel
emptyDtmc dtmcmodel = dtmcmodel { graphs = [] }

-- Parser for fDTMC and order DTMCs
-- Transverse the DTMC resolving variabilities
-- TODO: Thiago put your code here.
parseDtmcModel x = do 
               let cm = parseResult DtmcModel { graphs = [] }
               return cm

parseResult  g  = Success g
                

