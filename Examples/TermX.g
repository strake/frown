{-

This example illustrates the use of multiple attributes that are
simultaneously evaluated.

	frown TermX.g

Try

	start (lexer "87-(8+23)") :: IO (Int,Expr)

-}

module TermX
where
import Char

type Op                       =  Int -> Int -> Int

data AddOp                    =  Plus  | Minus
                                 deriving (Show)

data Expr                     =  Add Expr AddOp Expr
                              |  Const Int
                                 deriving (Show)

type Result                   =  IO

%{

Terminal                      =  NAT {Int} | ADDOP {Op} {AddOp} | LPAR | RPAR | EOF;
Nonterminal                   =  start {Int} {Expr} | expr {Int} {Expr} | term {Int} {Expr};

start {v} {e}
    :  expr {v} {e}, EOF;
expr {v1 `f` v2} {Add e1 op e2}
    :  expr {v1} {e1}, ADDOP {f} {op}, term {v2} {e2};
expr {v} {e}
    :  term {v} {e};
term {n} {Const n}
    :  NAT {n};
term {v} {e}
    :  LPAR, expr {v} {e}, RPAR;

}%

frown ts                      =  fail "syntax error"

data Terminal                 =  NAT Int | ADDOP Op AddOp | LPAR | RPAR | EOF

lexer                         :: String -> [Terminal]
lexer []                      =  [EOF]
lexer ('+' : cs)              =  ADDOP (+) Plus  : lexer cs
lexer ('-' : cs)              =  ADDOP (-) Minus : lexer cs
lexer ('(' : cs)              =  LPAR : lexer cs
lexer (')' : cs)              =  RPAR : lexer cs
lexer (c : cs)
    | isDigit c               =  let (n, cs') = span isDigit cs
                                 in  NAT (read (c : n)) : lexer cs'
    | otherwise               =  lexer cs
