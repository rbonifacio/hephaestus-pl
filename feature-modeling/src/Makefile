all:
	happy -gca FeatureModel/Parsers/FMIde/ParFMIde.y
	alex -g FeatureModel/Parsers/FMIde/LexFMIde.x
	(cd FeatureModel/Parsers/FMIde/; latex DocFMIde.tex; dvips DocFMIde.dvi -o DocFMIde.ps)
	ghc --make FeatureModel/Parsers/FMIde/TestFMIde.hs -o FeatureModel/Parsers/FMIde/TestFMIde
clean:
	-rm -f FeatureModel/Parsers/FMIde/*.log FeatureModel/Parsers/FMIde/*.aux FeatureModel/Parsers/FMIde/*.hi FeatureModel/Parsers/FMIde/*.o FeatureModel/Parsers/FMIde/*.dvi
	-rm -f FeatureModel/Parsers/FMIde/DocFMIde.ps
distclean: clean
	-rm -f FeatureModel/Parsers/FMIde/DocFMIde.* FeatureModel/Parsers/FMIde/LexFMIde.* FeatureModel/Parsers/FMIde/ParFMIde.* FeatureModel/Parsers/FMIde/LayoutFMIde.* FeatureModel/Parsers/FMIde/SkelFMIde.* FeatureModel/Parsers/FMIde/PrintFMIde.* FeatureModel/Parsers/FMIde/TestFMIde.* FeatureModel/Parsers/FMIde/AbsFMIde.* FeatureModel/Parsers/FMIde/TestFMIde FeatureModel/Parsers/FMIde/ErrM.* FeatureModel/Parsers/FMIde/SharedString.* FeatureModel/Parsers/FMIde/FMIde.dtd FeatureModel/Parsers/FMIde/XMLFMIde.* Makefile*
	-rmdir -p FeatureModel/Parsers/FMIde/
