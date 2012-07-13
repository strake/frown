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

.PHONY: test clean distclean

test-frown: TestFrown.lhs
ifeq ($(hc),nhc98)
	hmake -nhc98 -98 +CTS -H32M -CTS TestFrown
	mv TestFrown test-frown
else
	ghc --make -o test-frown TestFrown.lhs
endif

test: test-frown
	@echo "**** testing small grammars"
	./test-frown
	@echo "**** testing example grammars"
	cd ../Manual/Examples && $(MAKE) test
	@echo "**** testing Haskell parser"
	cd Haskell && $(MAKE) test

clean:
	@rm -f *.o *.hi
	cd Haskell && $(MAKE) clean

distclean: clean
	@rm -f *.hs*
	@rm -f test-frown
	cd Haskell && $(MAKE) distclean