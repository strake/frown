{-

This example demonstrates the use of error correction. A closing brace
is automatically inserted if it is missing.

	frown --debug Light.g
	frown --debug --trace Light.g

Try

	decls [LBRACE, Var, EQU, Var, EOF] :: IO ()
        expr [Var, LPAR, LET, LBRACE, Var, EQU, Var, IN, Var, RPAR, EOF] :: IO ()

-}

module Light
where
import Monad

type Result                   =  Maybe

%{

Terminal                      =  LBRACE
                              |  RBRACE
                              |  SEMICOLON
                              |  EQU
                              |  LET
                              |  IN
                              |  LPAR
                              |  RPAR
                              |  Var
                              | *EOF;

Nonterminal                   = *decls
                              |  declList
                              |  decl
                              |  close
                              | *expr
                              |  fexpr
                              |  aexpr;

decls                         :  LBRACE, declList, close;

declList                      :  declList, SEMICOLON, decl;
                              |  declList, SEMICOLON;
                              |  decl;
                              |  ;

decl                          :  Var, EQU, expr;

close                         :  RBRACE;
                              |  insert RBRACE;

expr                          :  LET, decls, IN, expr;
                              |  fexpr;

fexpr                         :  fexpr, aexpr;
                              |  aexpr;

aexpr                         :  Var;
                              |  LPAR, expr, RPAR;
}%


data Terminal                 =  LBRACE
                              |  RBRACE
                              |  SEMICOLON
                              |  EQU
                              |  LET
                              |  IN
                              |  LPAR
                              |  RPAR
                              |  Var
                              |  EOF
                                 deriving (Show)

frown _                       =  fail "syntax error"