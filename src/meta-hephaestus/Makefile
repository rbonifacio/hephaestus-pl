options = -i../feature-modeling/src:../core/src:../configuration-knowledge/src:../funsat-0.6.2/src -XDeriveDataTypeable

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
	make LoadBaseProduct.drive
	make LoadHephaestus.drive
	make LoadTestProduct.drive

test-build:
	make BuildTestProduct.drive

test-bootstrap:
	make BuildHephaestus.drive

%.drive:
	ghc --make HplDrivers/$* ${options}
	HplDrivers/$*
