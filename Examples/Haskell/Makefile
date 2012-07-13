ifeq ($(hc),nhc98)
main :=	hmake -nhc -c -M120M -K32M +CTS -H32M -CTS -ILib -LLib Main; mv Main a.out
else
ifeq ($(hc),ghc)
#main :=	ghc --make -iLib -LLib Main.lhs +RTS -c -M120M
main :=	ghc --make -iLib -LLib Main.lhs
else
#main :=	ghc --make -package text -package util Main.lhs +RTS -c -M120M
main :=	ghc --make -package text -package util Main.lhs
endif
endif

define run
	$(main)
	./a.out Input/Big.hs | diff Input/Big.out -
	./a.out Input/Generate.hs | diff Input/Generate.out -	
	./a.out Test/Small1.hs | diff Test/Small1.out -	
	./a.out Test/Small2.hs | diff Test/Small2.out -	
	./a.out Test/Small3.hs | diff Test/Small3.out -	
	./a.out Test/Small4.hs | diff Test/Small4.out -	
	./a.out Test/Small5.hs | diff Test/Small5.out -	
	./a.out Test/Small6.hs | diff Test/Small6.out -	
endef

INPUTS          := $(wildcard Input/*.hs)
HC_OPTIONS	= -O
FROWN_OPTIONS	= --optimize

.PHONY: test clean distclean

test:
	@echo "*** standard"
	../../frown --lexer --expected --signature $(FROWN_OPTIONS) --code=standard HsParser.lg
	$(run)
	@echo "*** compact"
	../../frown --lexer --expected --signature $(FROWN_OPTIONS) --code=compact HsParser.lg
	$(run)
	@echo "*** stackless"
	../../frown --lexer --expected --signature $(FROWN_OPTIONS) --code=stackless HsParser.lg
	$(run)
#	@echo "*** gvstack"
#	../../frown --lexer --expected --signature $(FROWN_OPTIONS) --code=gvstack HsParser.lg
#	$(run)

clean:
	@rm -f *~ *.o *.hi Lib/*.o Lib/*.hi

distclean: clean
	@rm -f HsParser.hs* a.out