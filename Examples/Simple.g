{-

	frown Simple.g

Try

	expr (lexer "3+(5)") :: IO Expr

-}

module Simple
where
import Char

data Expr                     =  Const Int
                              |  Add Expr Expr
                                 deriving (Show)

type Result                   =  IO

%{

Terminal                      =  NAT {Int} | PLUS | LPAR | RPAR;

Nonterminal                   =  *expr {Expr} | term {Expr};

expr  {Add e1 e2}             :  expr {e1}, PLUS, term {e2};
      {e}                     |  term {e};
term  {Const n}               :  NAT {n};
      {e}                     |  LPAR, expr {e}, RPAR;

}%

frown ts                      =  fail ("syntax error: " ++ unlex ts ++ ".")

data Terminal                 =  NAT Int | PLUS | LPAR | RPAR | EOF
                                 deriving (Show)

lexer                         :: String -> [Terminal]
lexer []                      =  []
lexer ('+' : cs)              =  PLUS : lexer cs
lexer ('(' : cs)              =  LPAR : lexer cs
lexer (')' : cs)              =  RPAR : lexer cs
lexer (c : cs)
    | isDigit c               =  let (n, cs') = span isDigit cs
                                 in  NAT (read (c : n)) : lexer cs'
    | otherwise               =  lexer cs

unlex []                      =  []
unlex (PLUS  : ts)            =  '+' : unlex ts
unlex (LPAR  : ts)            =  '(' : unlex ts
unlex (RPAR  : ts)            =  ')' : unlex ts
unlex (NAT n : ts)            =  show n ++ unlex ts

