{-

A grammar with an unreachable nonterminal symbol (`y').

-}

module Dead
where

type Terminal                   =  Char

type Result                     =  []

%{

Terminal                        =  'a' | 'b' | 'c' | 'd';
Nonterminal                     =   s  |  x  |  y;

s :  x, 'a';
  |  'b';

x : 'c';

y : 'd'; -- unreachable

}%

frown ts                        =  fail "syntax error"
