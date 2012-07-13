{-

The standard example: arithmetic expressions (see dragon book p222).

	frown --debug Test.g

Try
	expr (lexer "a+b*c+d") :: [Expr]

Tracing the parse:

	frown --debug --trace Expr.g

Try
	expr (lexer "a+b*c+d")

-}

module Test
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

Terminal                      =  IDENT {String}
                              |  ADDOP {AddOp}
                              |  MULOP {MulOp}
                              |  LPAR
                              |  RPAR
                              | *EOF;

Nonterminal                   = *expr {Expr}
                              |  term {Expr}
                              |  factor {Expr};

expr   {Add e1 op e2}         :  expr {e1}, ADDOP {op}, term {e2};
       {e}                    |  term {e};
term   {Mul e1 op e2}         :  term {e1}, MULOP {op}, factor {e2};
       {e}                    |  factor {e};
factor {e}                    :  LPAR, expr {e}, RPAR;
       {Id s}                 |  IDENT {s};

}%

frown ts                      =  fail "syntax error"

data Terminal                 =  IDENT String | ADDOP AddOp | MULOP MulOp | LPAR | RPAR | EOF
                                 deriving (Show)

lexer                         :: String -> [Terminal]
lexer []                      =  [EOF]
lexer ('+' : cs)              =  ADDOP Plus   : lexer cs
lexer ('-' : cs)              =  ADDOP Minus  : lexer cs
lexer ('*' : cs)              =  MULOP Times  : lexer cs
lexer ('/' : cs)              =  MULOP Divide : lexer cs
lexer ('(' : cs)              =  LPAR : lexer cs
lexer (')' : cs)              =  RPAR : lexer cs
lexer (c : cs)
    | isAlpha c               =  let (n, cs') = span isAlphaNum cs
                                 in  IDENT (c : n) : lexer cs'
    | otherwise               =  lexer cs
