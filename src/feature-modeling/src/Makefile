all:
	happy -gca FeatureModel/Parsers/FMGrammar/ParFMGrammar.y
	alex -g FeatureModel/Parsers/FMGrammar/LexFMGrammar.x
	ghc --make FeatureModel/Parsers/FMGrammar/TestFMGrammar.hs -o FeatureModel/Parsers/FMGrammar/TestFMGrammar

clean:
	-rm -f FeatureModel/Parsers/FMGrammar/*.log FeatureModel/Parsers/FMGrammar/*.aux FeatureModel/Parsers/FMGrammar/*.hi FeatureModel/Parsers/FMGrammar/*.o FeatureModel/Parsers/FMGrammar/*.dvi
	-rm -f FeatureModel/Parsers/FMGrammar/DocFMGrammar.ps

distclean: clean
	-rm -f FeatureModel/Parsers/FMGrammar/DocFMGrammar.* FeatureModel/Parsers/FMGrammar/LexFMGrammar.* FeatureModel/Parsers/FMGrammar/ParFMGrammar.* FeatureModel/Parsers/FMGrammar/LayoutFMGrammar.* FeatureModel/Parsers/FMGrammar/SkelFMGrammar.* FeatureModel/Parsers/FMGrammar/PrintFMGrammar.* FeatureModel/Parsers/FMGrammar/TestFMGrammar.* FeatureModel/Parsers/FMGrammar/AbsFMGrammar.* FeatureModel/Parsers/FMGrammar/TestFMGrammar FeatureModel/Parsers/FMGrammar/ErrM.* FeatureModel/Parsers/FMGrammar/SharedString.* FeatureModel/Parsers/FMGrammar/ComposOp.* FeatureModel/Parsers/FMGrammar/FMGrammar.dtd FeatureModel/Parsers/FMGrammar/XMLFMGrammar.* Makefile*
		-rmdir -p FeatureModel/Parsers/FMGrammar/

