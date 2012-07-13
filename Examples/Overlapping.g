{-

This grammar illustrates overlapping patterns (terminals). Note that
the patterns are ordered by generality (most general one last).

	frown Overlapping

-}

module Overlapping
where
import Char

data Expr                     =  Var String
                              |  If Expr Expr Expr
	                         deriving (Show)

type Result                   =  Maybe

frown ts                      =  fail "syntax error"

%{

Terminal                      =  "if"   =  Ident "if"
                              |  "then" =  Ident "then"
                              |  "else" =  Ident "else"
                              |            Ident {String};

Nonterminal                   =  expr {Expr};

expr {Var s}                  :  Ident {s};
     {If e1 e2 e3}            |  "if", expr {e1}, "then", expr {e2}, "else", expr {e3};

}%

data Terminal                 =  Ident String
	                         deriving (Show)

lexer                         :: String -> [Terminal]
lexer []                      =  []
lexer (c : cs)
    | isAlpha c               =  let (n, cs') = span isAlphaNum cs
                                 in  Ident (c : n) : lexer cs'
    | otherwise               =  lexer cs
