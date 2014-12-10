module HplAssets.DTMC.Types
where 

import BasicTypes
import Data.Generics

import Data.FDTMC


-- SelectDTMC

data DtmcTransformation = SelectDTMC [Id]
			deriving (Show, Eq, Ord)

data DtmcModel = DtmcModel {
      dtmcs :: [Dtmc]
} deriving (Show, Eq, Typeable)


data Dtmc = Dtmc {
      dtmcId :: Id, 
      chain :: FDTMC
--TODO: put more things here.
} deriving (Show, Typeable)

data Value = Unbound | Value String
  deriving(Eq, Ord, Show, Data, Typeable)

-- The equality test of a dtmc is based on 
-- its id.
instance Eq Dtmc where 
 x == y = ((dtmcId x) == (dtmcId y))
