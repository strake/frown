.PHONY: test man clean

sources         := $(wildcard *.lg)
sessions	:= $(sources:.lg=.session)

#-----------------------------------------------------------------------------
# pattern rules

%.out : %.lg %.in test-frown
	./test-frown --generate $<

%.session : %.out sessions
	./sessions $<

#-----------------------------------------------------------------------------
# main target

all: $(sessions)

sessions: Sessions.lhs
ifeq ($(hc),nhc98)
	hmake -nhc98 -98 +CTS -H32M -CTS Sessions
	mv Sessions sessions
else
	ghc --make -o sessions Sessions.lhs
endif

# Note: all grammar files must be listed in `TestFrown.lhs'.

test-frown: TestFrown.lhs
ifeq ($(hc),nhc98)
	hmake -nhc98 -98 +CTS -H32M -CTS TestFrown
	mv TestFrown test-frown
else
	ghc --make -o test-frown TestFrown.lhs
endif

test: test-frown
	@echo "**** testing small grammars"
	@./test-frown

man: $(sessions)

clean:
	@rm -f *.hs*
	@rm -f *.o *.hi test-frown sessions