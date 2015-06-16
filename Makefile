none:

clean:
	@find . -name "*.o" -exec rm -rf {} \;
	@find . -name "*.hi" -exec rm -rf {} \;
	@cd src/meta-hephaestus; make clean
	
%-build:
	@cd src/meta-hephaestus; make $*-build
	
test:
	@cd src/meta-hephaestus; make test
	
test-products:
	make productUcmBpm-build
	make productReq-build	
	make productDtmc-build
	make productCloud-build	

push:
	git commit -a
	git push
