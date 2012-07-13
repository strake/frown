{-

This example demonstrates that the computation of the look-ahead
information must use a stack of transitions (rather than a single
transition). If we have several epsilon-reductions in a row, we must
be able to backtrack to the original state.

	frown LA.g

-}

module LA
where

type Terminal                   =  Char

type Result                     =  []

%{

Terminal                        =  'a' | 'b' | 'c' | 'x' | 'y';
Nonterminal                     =   s  |  a  |  b | c | e1 | e2;

s  : 'a', a, 'x';
   | 'a', c, 'y';
   | 'b', a, 'y';
   | 'b', c, 'x';

a  : b, e1, e2;

b  : 'c';

c  : 'c';

e1 : ;

e2 : ;

}%

frown ts                        =  fail "syntax error"
