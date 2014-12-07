module HplAssets.DTMC.Types
where 

import BasicTypes
import Data.Generics

data DtmcTransformation = SelectDTMC Id
                        | EvaluateExpr Id
                        | BindParameterDTMC Name Value
			deriving (Show, Eq, Ord)

data DtmcModel = DtmcModel {
      graphs :: [Dtmc]
} --deriving (Show, Eq, Data, Typeable)


data Dtmc = Dtmc {
      dtmcId :: Id, 
      dtmcName :: String
--TODO: put more things here.
} --deriving (Show, Data, Typeable)

data Value = Unbound | Value String
  deriving(Eq, Ord, Show, Data, Typeable)
