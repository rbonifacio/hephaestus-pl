\begin{code}
module FeatureModel.Parsers.FMPlugin.XmlFeatureParser where

import Text.XML.HXT.Core
import System.Environment

import qualified FeatureModel.Types as Base
import FeatureModel.Parsers.FMPlugin.XmlFeatureModel 

instance XmlPickler XmlFeature where
	xpickle = xpFeature

instance XmlPickler XmlGroupFeature where 
	xpickle = xpGroup 

instance XmlPickler XmlFeatureConfiguration where
        xpickle = xpFeatureConfiguration
	
uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a, b, c, d, e) -> f
uncurry5 fn (a, b, c, d, e) = fn a b c d e 

uncurry6 :: (a -> b -> c -> d -> e -> f -> g) -> (a, b, c, d, e, f) -> g
uncurry6 fn (a, b, c, d, e, f) = fn a b c d e f

xpFeature :: PU XmlFeature
xpFeature =
	xpElem "feature" $
	xpWrap (\ ((i,m1,m2),(n,c,g)) -> XmlFeature i m1 m2 n c g , 
                \t -> ((featureId t, cmin t, cmax t), (name t, children t, group t))) $
	xpPair (xpTriple (xpAttr "id" xpText)
	       (xpAttr "min" xpickle)
	       (xpAttr "max" xpickle))
	       (xpTriple (xpAttr "name" xpText)
	       (xpOption (xpList xpFeature)) 
	       (xpOption (xpGroup)))		 
			 
xpGroup :: PU XmlGroupFeature
xpGroup = 	
	xpElem "featureGroup" $
	xpWrap ( uncurry3 XmlGroupFeature, \ (XmlGroupFeature cmin cmax options) -> (cmin, cmax, options) ) $
	xpTriple ( xpAttr "min" xpickle ) 
	         ( xpAttr "max" xpickle ) 
	         ( xpList xpFeature )

xpFeatureConfiguration :: PU XmlFeatureConfiguration
xpFeatureConfiguration = 
        xpElem "feature" $
        xpWrap (uncurry3 XmlFeatureConfiguration, \ (XmlFeatureConfiguration cId cName cChildren) -> (cId, cName, cChildren)) $
        xpTriple ( xpAttr "id" xpText)
                 ( xpAttr "name" xpText)
                 ( xpOption (xpList xpFeatureConfiguration))
        
\end{code}