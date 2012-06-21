options = -i../feature-modeling/src:../core/src:../configuration-knowledge/src -fglasgow-exts 

#
# By running "make test" twice, bootstrapping is both completed and tested.
# If bootstrapping fails, see HplProducts/Baselines for versions to be restored.
#
test:
#	make LoadBaseProduct.drive
	make BuildTestProduct.drive
#	make LoadTestProduct.drive
#	make BuildHephaestus.drive
#	make LoadHephaestus.drive

%.drive:
	@echo main | ghci HplDrivers.$* ${options}

