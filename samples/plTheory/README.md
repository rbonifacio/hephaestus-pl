### PL-Theory

To run this sample, go to the source folder (../../src/meta-hephaestus)  
and build the code version of hephaestus:

* make productCode-build
* make buildExecutable

After that, you can run the specific version of hephaestus 
using GHCi (interpreter mode)

* make runProduct

Several modules should be loaded (the interface of GHCi 
must present something like: Ok, modules loaded: BasicTypes, ...). 
Using the GHCi prompt, enter > main and the absolute 
path to the project.properties file, like: 


* /{user+home}/workspace/hephaestus-pl/samples/plTheory/project.properties


We have specified one instance of this product line (productConfiguration01), 
with the following features:

+ PLTheory
  + mpl
  + fm 
    + intermediateFM
    + propositionalFM
  + assets
    + renaming
  + ck 
  + populations

### Known issues

For this configuration, the following issues have been observed: 

  * FeatureModel.prf is missing
  
