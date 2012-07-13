{-

Illustrates the use of patterns with embedded character literals.

	frown VarExpr.g

-}

module VarExpr
where
import Char

data AddOp                    =  Plus  | Minus
                                 deriving (Show)
data MulOp                    =  Times | Divide
                                 deriving (Show)

data Expr                     =  Add Expr AddOp Expr
                              |  Mul Expr MulOp Expr
                              |  Id String
                                 deriving (Show)

type Result                   =  []

%{

Terminal                      =  IDENT {String} | ADDOP {AddOp} | MULOP {MulOp} | Special '(' | Special ')';
Nonterminal                   =  expr {Expr} | term {Expr} | factor {Expr};

expr {Add e1 op e2}           :  expr {e1}, ADDOP {op}, term {e2};
     {e}                      |  term {e};
term {Mul e1 op e2}           :  term {e1}, MULOP {op}, factor {e2};
     {e}                      |  factor {e};
factor {e}                    :  Special '(', expr {e}, Special ')';
       {Id s}                 |  IDENT {s};

}%

frown _                       =  fail "syntax error"

data Terminal                 =  IDENT String | ADDOP AddOp | MULOP MulOp | Special Char

lexer                         :: String -> [Terminal]
lexer []                      =  []
lexer ('+' : cs)              =  ADDOP Plus   : lexer cs
lexer ('-' : cs)              =  ADDOP Minus  : lexer cs
lexer ('*' : cs)              =  MULOP Times  : lexer cs
lexer ('/' : cs)              =  MULOP Divide : lexer cs
lexer ('(' : cs)              =  Special '(' : lexer cs
lexer (')' : cs)              =  Special ')' : lexer cs
lexer (c : cs)
    | isAlpha c               =  let (n, cs') = span isAlphaNum cs
                                 in  IDENT (c : n) : lexer cs'
    | otherwise               =  lexer cs
