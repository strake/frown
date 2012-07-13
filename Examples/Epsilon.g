{-

Example of an empty production.

	frown Epsilon.g

Try

	start "aaaa" :: [String]

-}

module Epsilon
where

type Terminal                 =  Char

type Result                   =  []

%{

Terminal                      =  'a';
Nonterminal                   =  start {String};

start {[]}                    :  ;
start {'a' : as}              :  'a', start {as};

}%

frown ts                      =  fail "syntax error"