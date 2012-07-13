{-

Test case for oversimplification of look-ahead computation.
See, DeRemer, p.632.

	frown Conflict2.g

-}

module Conflict
where

type Terminal                 =  Char

type Result                   =  IO

%{

Terminal                      =  'a' | 'b' | 'c' | 'd' | 'g';
Nonterminal                   =   s  |  a  |  b;

s : 'a', a, 'c';
  | 'b', a, 'd';
  | 'a', 'g', 'd';
  | 'b', 'g', 'c';

a : b;

b : 'g';

}%

frown ts                      =  fail "syntax error"
