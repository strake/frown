{-

A simple desk calculator.

	frown Calc.g

Try

	expr (lexer "1+2*4") :: [Int]

-}


module Calc
where
import Char

instance Show (a -> b) where show _ = "<function>"

type Op                       =  Int -> Int -> Int

type Result                   =  []

%{

Terminal                      =  NAT {Int} | ADDOP {Op} | MULOP {Op} | LPAR | RPAR | *EOF;
Nonterminal                   = *expr {Int} | term {Int} | factor {Int};

expr {v1 `op` v2}             :  expr {v1}, ADDOP {op}, term {v2};
expr {e}                      :  term {e};
term {v1 `op` v2}             :  term {v1}, MULOP {op}, factor {v2};
term {e}                      :  factor {e};
factor {e}                    :  LPAR, expr {e}, RPAR;
factor {n}                    :  NAT {n};

}%

frown ts                      =  fail ("syntax error: " ++ show ts)

data Terminal                 =  NAT Int | ADDOP Op | MULOP Op | LPAR | RPAR | EOF
                                 deriving (Show)

lexer                         :: String -> [Terminal]
lexer []                      =  [EOF]
lexer ('+' : cs)              =  ADDOP (+) : lexer cs
lexer ('-' : cs)              =  ADDOP (-) : lexer cs
lexer ('*' : cs)              =  MULOP (*) : lexer cs
lexer ('/' : cs)              =  MULOP div : lexer cs
lexer ('(' : cs)              =  LPAR : lexer cs
lexer (')' : cs)              =  RPAR : lexer cs
lexer (c : cs)
    | isDigit c               =  let (n, cs') = span isDigit cs
                                 in  NAT (read (c : n)) : lexer cs'
    | otherwise               =  lexer cs
