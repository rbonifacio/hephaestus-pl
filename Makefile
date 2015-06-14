none:

clean:
	@find . -name "*.o" -exec rm -rf {} \;
	@find . -name "*.hi" -exec rm -rf {} \;
	@cd src/meta-hephaestus; make clean
	
%-build:
	@cd src/meta-hephaestus; make $*-build	

push:
	git commit -a
	git push
