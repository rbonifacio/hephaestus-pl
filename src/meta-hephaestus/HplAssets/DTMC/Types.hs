module HplAssets.DTMC.Types
where 

import BasicTypes
import Data.Generics

import Data.FDTMC


-- SelectDTMC

data DtmcTransformation = SelectDTMC [Id] |
                        AppendDTMC Id Pointcut |
                        ComposeDTMC Id Pointcut Pointcut 
			deriving (Show, Eq, Ord)

data DtmcModel = DtmcModel {
      dtmcs :: [Dtmc]
} deriving (Show, Eq, Typeable)


data Dtmc = Dtmc {
      dtmcId :: Id, 
      chain :: FDTMC
} deriving (Show, Typeable)


type Pointcut = String

-- The equality test of a dtmc is based on 
-- its id.
instance Eq Dtmc where 
 x == y = ((dtmcId x) == (dtmcId y))
