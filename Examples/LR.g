{-

This grammar is LR(1) but not LALR(1). However, thanks to the stack
information there is no reduce/reduce conflict. In the traditional
setting only the look-ahead information and the current state is used
to determine the reducing production. Here, we can use also the past,
ie the path to the current state.

Exercise 4.40, dragon book.

	frown --debug LR.g

-}

module LR
where

type Terminal                   =  Char

type Result                     =  []

%{

Terminal                        =  'a' | 'b' | 'c' | 'd';
Nonterminal                     =   s  |  x  |  y;

s :  x, 'a';
  | 'b', x, 'c';
  |  y, 'c';
  | 'b', y, 'a';

x : 'd';

y : 'd';

}%

frown ts                        =  fail "syntax error"
