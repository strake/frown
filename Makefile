###############################################################################
#                                                                             #
#   Frown --- An LALR(k) parser generator for Haskell 98                      #
#   Copyright (C) 2001-2005 Ralf Hinze                                        #
#                                                                             #
#   This program is free software; you can redistribute it and/or modify      #
#   it under the terms of the GNU General Public License (version 2) as       #
#   published by the Free Software Foundation.                                #
#                                                                             #
#   This program is distributed in the hope that it will be useful,           #
#   but WITHOUT ANY WARRANTY; without even the implied warranty of            #
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             #
#   GNU General Public License for more details.                              #
#                                                                             #
#   You should have received a copy of the GNU General Public License         #
#   along with this program; see the file COPYING.  If not, write to          #
#   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,          #
#   Boston, MA 02111-1307, USA.                                               #
#                                                                             #
#   Contact information                                                       #
#   Email:      Ralf Hinze <ralf@cs.uni-bonn.de>                              #
#   Homepage:   http://www.informatik.uni-bonn.de/~ralf/                      #
#   Paper mail: Dr. Ralf Hinze                                                #
#               Institut für Informatik III                                   #
#               Universität Bonn                                              #
#               Römerstraße 164                                               #
#               53117 Bonn, Germany                                           #
#                                                                             #
###############################################################################

#-----------------------------------------------------------------------------
# directories and files

export version	:= 0.6

export PREFIX	:= /usr/local
export bin      := $(PREFIX)/bin
export doc	:= $(PREFIX)/share/doc/frown-$(version)

main		:= frown
sources         := $(wildcard *.lhs)

#-----------------------------------------------------------------------------
# commands

hc		:= ghc

#-----------------------------------------------------------------------------
# main target

$(main) : $(sources) GParser2.hs
ifeq ($(hc),nhc98)
	hmake -nhc98 -98 +CTS -H32M -CTS -ILib -PLib Main
	mv Main $(main)
else
ifeq ($(hc),ghc)
	$(hc) -XLambdaCase --make -O2 -iLib -LLib -o $(main) Main.lhs
else
	ghc -XLambdaCase --make -Wall -O2 -package util -package rh -o $(main) Main.lhs
endif
endif
#	strip $(main)

#-----------------------------------------------------------------------------
# rules

.PHONY : test man install all clean distclean

test: $(main)
	cd Examples && $(MAKE) test

man:
	cd Manual && $(MAKE) man

install: $(main)
	cp $(main) $(bin)
	cd Manual && $(MAKE) install

all:
	$(MAKE) install
	$(MAKE) test
	$(MAKE) man

clean:
	@rm -f *~ *.o *.hi Lib/*.o Lib/*.hi
	cd Examples && $(MAKE) clean
	cd Manual   && $(MAKE) clean

distclean: clean
	@rm -f *.hs[0-9]*
	@rm -f frown
	cd Examples && $(MAKE) distclean
	cd Manual   && $(MAKE) distclean

GParser2.hs: GParser2.lg
	frown --expected --signature -l get -Occompact GParser2.lg
