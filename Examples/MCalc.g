{-

This example demonstrates monadic semantic actions.

	frown MCalc.g

Try 
	expr (lexer "6*8+5")

-}


module MCalc
where
import Char

type Op                       =  Int -> Int -> Int

type Result                   =  IO

%{

Terminal                      =  NAT {Int} | ADDOP {Op} | MULOP {Op} | LPAR | RPAR | *EOF;
Nonterminal                   =  expr {Int} | term {Int} | factor {Int};

expr {%trace (v1 `op` v2)}    :  expr {v1}, ADDOP {op}, term {v2};
     {e}                      |  term {e};
term {%trace (v1 `op` v2)}    :  term {v1}, MULOP {op}, factor {v2};
     {e}                      |  factor {e};
factor {e}                    :  LPAR, expr {e}, RPAR;
       {n}                    |  NAT {n};

}%

frown ts                      =  fail "syntax error"

trace v                       =  print v >> return v

data Terminal                 =  NAT Int | ADDOP Op | MULOP Op | LPAR | RPAR | EOF

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
