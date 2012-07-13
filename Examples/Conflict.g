{-

This grammar is LALR(1). The example demonstrates that the computation
of the look ahead information in the Duponcheel/Swierstra paper is too
coarse, that is, we have a shift/reduce conflict.

	frown Conflict.g

Try

	s "cb" >>= print
	s "aacbb" >>= print
	s "aacb" >>= print
-}

module Conflict
where

type Terminal                 =  Char

type Result                   =  IO

%{

Terminal                      =  'a' | 'b' | 'c';
Nonterminal                   =   s  |  x  |  y;

s : x;
  | 'c', 'b';

x : 'a', x, 'b';
  | y;

y : 'c';

}%

frown ts                      =  fail "syntax error"
