{-

A grammar with an `empty' nonterminal (that is, a nonterminal that
denotes the empty language).

	frown Empty.g

-}

module Empty
where

type Terminal                 =  Char

type Result                   =  Maybe

%{

Nonterminal                   =   s;

}%

frown ts                      =  fail "syntax error"
