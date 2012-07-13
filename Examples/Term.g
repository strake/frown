{-

This example illustrates multiple entry point.

	frown Term.g

Try

	expr (lexer "1+5") :: IO Expr
	term (lexer "(1+5)") :: IO Expr

-}

module Term
where
import Char

data AddOp                    =  Plus  | Minus
                                 deriving (Show)

data Expr                     =  Add Expr AddOp Expr
                              |  Const Int
                                 deriving (Show)

type Result                   =  IO

%{

Terminal                      =  NAT {Int} | ADDOP {AddOp} | LPAR | RPAR | *EOF;
Nonterminal                   =  *expr {Expr} | *term {Expr};

expr  {Add e1 op e2}          :  expr {e1}, ADDOP {op}, term {e2};
      {e}                     |  term {e};
term  {Const n}               :  NAT {n};
      {e}                     |  LPAR, expr {e}, RPAR;

}%

frown ts                      =  fail ("syntax error: " ++ unlex ts ++ ".")

data Terminal                 =  NAT Int | ADDOP AddOp | LPAR | RPAR | EOF
                                 deriving (Show)

lexer                         :: String -> [Terminal]
lexer []                      =  [EOF]
lexer ('+' : cs)              =  ADDOP Plus   : lexer cs
lexer ('-' : cs)              =  ADDOP Minus  : lexer cs
lexer ('(' : cs)              =  LPAR : lexer cs
lexer (')' : cs)              =  RPAR : lexer cs
lexer (c : cs)
    | isDigit c               =  let (n, cs') = span isDigit cs
                                 in  NAT (read (c : n)) : lexer cs'
    | otherwise               =  lexer cs

unlex []                      =  []
unlex [EOF]                   =  "<end of input>"
unlex (ADDOP Plus  : ts)      =  '+' : unlex ts
unlex (ADDOP Minus : ts)      =  '-' : unlex ts
unlex (LPAR        : ts)      =  '(' : unlex ts
unlex (RPAR        : ts)      =  ')' : unlex ts
unlex (NAT n       : ts)      =  show n ++ unlex ts

