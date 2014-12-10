module HplAssets.DTMC.Parsers.Dot (parseDtmcModel)
where 

import BasicTypes

import HplAssets.DTMC.Types

emptyDtmc :: DtmcModel -> DtmcModel
emptyDtmc dtmcmodel = dtmcmodel { dtmcs = [] }

-- Parser for fDTMC and order DTMCs
-- Transverse the DTMC resolving variabilities
-- TODO: Thiago put your code here.
parseDtmcModel x = do 
               let cm = parseResult DtmcModel { dtmcs = [] }
               return cm

parseResult  g  = Success g
