options = -i../feature-modeling/src:../core/src:../configuration-knowledge/src:../../../funsat/src -XDeriveDataTypeable -XMultiParamTypeClasses

bnfc = /Users/hephaestus/Library/Haskell/bin/bnfc

#
# By running "make test" twice, bootstrapping is both completed and tested.
# If bootstrapping fails, see HplProducts/Baselines for versions to be restored.
#
test:
	make test-load
	make test-build
	make test-bootstrap

test-load:
	make LoadAssets.drive
	make LoadBase.drive
	make LoadHephaestus.drive
	make LoadTest.drive

test-build:
	make BuildTest.drive

productUcmBpm-build:
	make BuildUcmBpmProduct.drive

productReq-build:
	make BuildReqProduct.drive
	make LoadTest.drive

productCode-build:
	make BuildCodeProduct.drive

test-bootstrap:
	make BuildHephaestus.drive

buildExecutable:
	ghc --make HplProducts/Test.hs ${options}

runProduct:
	ghci HplProducts/Test.hs ${options}

buildParserForComponentModel:
	${bnfc} -p HplAssets.ComponentModel.Parsers ComponentModel.cf
	happy -gca HplAssets/ComponentModel/Parsers/ParComponentModel.y
	alex -g HplAssets/ComponentModel/Parsers/LexComponentModel.x
	ghc --make HplAssets/ComponentModel/Parsers/TestComponentModel.hs -o HplAssets/ComponentModel/Parsers/TestComponentModel

%.drive:
	ghc --make HplDrivers/$* ${options}
	HplDrivers/$*

clean:
	find . -name "*.o" -exec rm -rf {} \;
	find . -name "*.hi" -exec rm -rf {} \;
	cd HplDrivers; make clean

