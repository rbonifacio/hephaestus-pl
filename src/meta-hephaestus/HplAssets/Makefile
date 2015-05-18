all:
	happy -gca HplAssets/ComponentModel/Parsers/ParComponentModel.y
	alex -g HplAssets/ComponentModel/Parsers/LexComponentModel.x
	ghc --make HplAssets/ComponentModel/Parsers/TestComponentModel.hs -o HplAssets/ComponentModel/Parsers/TestComponentModel

clean:
	-rm -f HplAssets/ComponentModel/Parsers/*.log HplAssets/ComponentModel/Parsers/*.aux HplAssets/ComponentModel/Parsers/*.hi HplAssets/ComponentModel/Parsers/*.o HplAssets/ComponentModel/Parsers/*.dvi
	-rm -f HplAssets/ComponentModel/Parsers/DocComponentModel.ps

distclean: clean
	-rm -f HplAssets/ComponentModel/Parsers/DocComponentModel.* HplAssets/ComponentModel/Parsers/LexComponentModel.* HplAssets/ComponentModel/Parsers/ParComponentModel.* HplAssets/ComponentModel/Parsers/LayoutComponentModel.* HplAssets/ComponentModel/Parsers/SkelComponentModel.* HplAssets/ComponentModel/Parsers/PrintComponentModel.* HplAssets/ComponentModel/Parsers/TestComponentModel.* HplAssets/ComponentModel/Parsers/AbsComponentModel.* HplAssets/ComponentModel/Parsers/TestComponentModel HplAssets/ComponentModel/Parsers/ErrM.* HplAssets/ComponentModel/Parsers/SharedString.* HplAssets/ComponentModel/Parsers/ComposOp.* HplAssets/ComponentModel/Parsers/ComponentModel.dtd HplAssets/ComponentModel/Parsers/XMLComponentModel.* Makefile*
		-rmdir -p HplAssets/ComponentModel/Parsers/

