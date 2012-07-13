{-

This grammar is LR(1) but not LALR(1). However, thanks to the stack
information there is no reduce/reduce conflict.

Example 4.44, dragon book.

	frown LR1.g

-}

module LR1
where

type Terminal                   =  Char

type Result                     =  Maybe

%{

Terminal                        =  'a' | 'b' | 'c' | 'd' | 'e';
Nonterminal                     =   s  |  x |  y;

s : 'a', x, 'e';
  | 'a', y, 'd';
  | 'b', x, 'd';
  | 'b', y, 'e';

x : 'c';

y : 'c';

}%

frown ts                      =  fail "syntax error"
