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

file		:= Manual
sources         := $(Examples/wildcard *.lg)
sessions	:= $(sources:.lg=.session)

#-----------------------------------------------------------------------------
# commands

ghc		:= ghc
ghcflags	:= -Wall -O
flags		:= --poly
spell           := ispell -d british -t -l -p

#-----------------------------------------------------------------------------
# pattern rules

%.tex : %.lhs %.fmt $(sessions)
	lhs2TeX $(flags) $< > $@

%.ps : %.dvi
	dvips -o $@ $<

%.pdf : %.dvi
	dvipdfm $<

%.hi : %.o
	@:

%.out : %.lg %.in
	Examples/test-frown --generate $<

%.session : %.out
	Examples/sessions $<

#-----------------------------------------------------------------------------
# rules

.PHONY 	: man examples edit xdvi gv acro print spell spellupdate links clean distclean

man: $(file).ps $(file).pdf html

install: man
	mkdir -p $(doc)
	cp $(file).ps $(doc)
	cp $(file).pdf $(doc)
	cp -r html $(doc)

edit :
	xemacs $(file).fmt $(file).lhs &

examples:
	cd Examples && $(MAKE) man

# $(file).dvi depends on $(file).lhs rather than $(file).tex, so that
# it is not generated when $(file).tex is removed in the distribution.

$(file).dvi : $(file).lhs $(file).fmt $(sessions)
	$(MAKE) $(file).tex
	latex $(file)
	bibtex $(file)
	latex $(file)
	latex $(file)
	latex $(file)

xdvi : $(file).dvi
	xdvi -s 6 $(file).dvi &

gv : $(file).ps
	gv -spartan $(file).ps &

acro : $(file).pdf
	/opt/Acrobat7/acroread $(file).pdf &

html: $(file).lhs $(file).fmt $(sessions)
	lhs2TeX --math $(file).lhs > $(file).tex
	latex $(file)
	bibtex $(file)
	latex $(file)
	latex $(file)
	latex $(file)
# the above commands are only used to produce a `.bbl' file
	lhs2TeX --tt -lhtml=True $(file).lhs > $(file).tex
	hevea -fix $(file).tex
	hevea -fix $(file).tex
	hacha $(file).html
	imagen $(file)
	mkdir -p html
	mv index.html $(file).html $(file).css $(file)[0-9]*.html $(file)[0-9]*.gif *motif.gif html

spell: $(file).tex
	egrep -v '$\%' $(file).tex | $(spell) $(file).spell | sort | uniq

spellupdate: $(file).tex
	egrep -v '$\%' $(file).tex | $(spell)  $(file).spell | sort | uniq > $(file).spell

sessions: Examples/Sessions.lhs
	cd Examples && $(MAKE) sessions

test-frown: Examples/TestFrown.lhs
	cd Examples && $(MAKE) test-frown

links:
	ln -s $(HOME)/local/abbr.bib
	ln -s $(HOME)/local/rh.bib
	ln -s $(HOME)/local/lhs2TeX.fmt
	ln -s $(HOME)/local/lhs2TeX.sty
	ln -s $(HOME)/local/FuncMP.mp

clean:
	@rm -f *~ *.aux *.bbl *.blg *.log *.toc *.out
	@rm -f $(file).tex $(file).image.tex *.mpx
	cd Examples && $(MAKE) clean

distclean : clean
	@rm -f abbr.bib rh.bib lhs2TeX.fmt lhs2TeX.sty FuncMP.mp
