module TestBasicTypes 
where 

import BasicTypes

res = do
 m1 <- Success "abc"
 return $ m1