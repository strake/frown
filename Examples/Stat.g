{-

This example illustrates multiple entry point.

	frown Stat.g

Try

	stat (lexer "a=b") :: IO Stat
	expr (lexer "a") :: IO Expr

-}

module Stat
where
import Char

data Stat                     =  Assign String Expr
                                 deriving (Show)
data Expr                     =  Ident String
                                 deriving (Show)

type Result                   =  IO

%{

Terminal                      =  EQU | IDENT {String};
Nonterminal                   =  *stat {Stat} | *expr {Expr};

stat {Assign s e}             :  IDENT {s}, EQU, expr {e};
expr {Ident s}                :  IDENT {s};

}%

frown ts                      =  fail "syntax error"

data Terminal                 =  IF | THEN | ELSE | EQU | LPAR | RPAR | IDENT String

lexer                         :: String -> [Terminal]
lexer []                      =  []
lexer ('(' : cs)              =  LPAR : lexer cs
lexer (')' : cs)              =  RPAR : lexer cs
lexer ('=' : cs)              =  EQU  : lexer cs
lexer (c : cs)
    | isAlpha c               =  let (n, cs') = span isAlphaNum cs
                                 in  (case (c : n) of
                                          "if"   -> IF
                                          "then" -> THEN
                                          "else" -> ELSE
                                          s      -> IDENT s) : lexer cs'
    | otherwise               =  lexer cs
