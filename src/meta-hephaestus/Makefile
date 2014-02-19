options = -i../feature-modeling/src:../core/src:../configuration-knowledge/src:../funsat-0.6.2/src -XDeriveDataTypeable -XMultiParamTypeClasses

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

test-bootstrap:
	make BuildHephaestus.drive

%.drive:
	ghc --make HplDrivers/$* ${options}
	HplDrivers/$*

clean:
	find . -name "*.o" -exec rm -rf {} \;
	find . -name "*.hi" -exec rm -rf {} \;
	cd HplDrivers; make clean

