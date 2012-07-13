{-

Test case for oversimplification of look-ahead computation.
See, DeRemer, p.633.

	frown Conflict3.g

-}

module Conflict
where

type Terminal                 =  Char

type Result                   =  IO

%{

Terminal                      =  'a' | 'b' | 'c' | 'd' | 'g';
Nonterminal                   =   s  |  b  |  c  | d  | e;

s : 'a', b, 'd';
  | c, b, 'g';
  | 'c', 'd';

b : d, e;

c : 'c';

d : ;

e : ;

}%

frown ts                      =  fail "syntax error"
