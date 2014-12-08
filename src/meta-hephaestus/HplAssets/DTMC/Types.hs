module HplAssets.DTMC.Types
where 

import BasicTypes
import Data.Generics


-- SelectDTMC

data DtmcTransformation = SelectDTMC
			deriving (Show, Eq, Ord)

data DtmcModel = DtmcModel {
      graphs :: [Dtmc]
} deriving (Show, Eq, Data, Typeable)


data Dtmc = Dtmc {
      dtmcId :: Id, 
      dtmcName :: String
--TODO: put more things here.
} deriving (Show, Data, Typeable)

data Value = Unbound | Value String
  deriving(Eq, Ord, Show, Data, Typeable)

-- The equality test of a dtmc is based on 
-- its id.
instance Eq Dtmc where 
 x == y = ((dtmcId x) == (dtmcId y))
