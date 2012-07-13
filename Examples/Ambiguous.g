{-

An ambiguous grammar.

	frown --backtrack Ambiguous.g

Try

	expr (lexer "a+b+c+d") :: [Expr]

-}

module Ambiguous
where
import Char
import Monad

data AddOp                    =  Plus  | Minus
                                 deriving (Show)
data MulOp                    =  Times | Divide
                                 deriving (Show)

data Expr                     =  Add Expr AddOp Expr
                              |  Mul Expr MulOp Expr
                              |  Id String
                                 deriving (Show)

unparse (Id s)                =  s
unparse (Add e1 Plus   e2)    =  "(" ++ unparse e1 ++ "+" ++ unparse e2 ++ ")"
unparse (Add e1 Minus  e2)    =  "(" ++ unparse e1 ++ "-" ++ unparse e2 ++ ")"
unparse (Mul e1 Times  e2)    =  "(" ++ unparse e1 ++ "*" ++ unparse e2 ++ ")"
unparse (Mul e1 Divide e2)    =  "(" ++ unparse e1 ++ "/" ++ unparse e2 ++ ")"

type Result                   =  []

%{

Terminal                      =  Ident {String}
                              |  Addop {AddOp}
                              |  Mulop {MulOp}
                              |  LPar
                              |  RPar;

expr {Expr};
expr {Add e1 op e2}           :  expr {e1}, Addop {op}, expr {e2};
     {Mul e1 op e2}           |  expr {e1}, Mulop {op}, expr {e2};
     {e}                      |  LPar, expr {e}, RPar;
     {Id s}                   |  Ident {s};

}%

frown ts                      =  fail "syntax error"

data Terminal                 =  Ident String | Addop AddOp | Mulop MulOp | LPar | RPar

lexer                         :: String -> [Terminal]
lexer []                      =  []
lexer ('+' : cs)              =  Addop Plus   : lexer cs
lexer ('-' : cs)              =  Addop Minus  : lexer cs
lexer ('*' : cs)              =  Mulop Times  : lexer cs
lexer ('/' : cs)              =  Mulop Divide : lexer cs
lexer ('(' : cs)              =  LPar : lexer cs
lexer (')' : cs)              =  RPar : lexer cs
lexer (c : cs)
    | isAlpha c               =  let (n, cs') = span isAlphaNum cs
                                 in  Ident (c : n) : lexer cs'
    | otherwise               =  lexer cs
