{-

An ambiguous grammar made unambiguous using associativity and
precedences.

	frown PrecExpr.g

Try

	fmap unparse $ expr (lexer "a+b+c+d") :: Maybe String
	fmap unparse $ expr (lexer "a+b*c+d") :: Maybe String
	fmap unparse $ expr (lexer "a+b*c*d") :: Maybe String
	fmap unparse $ expr (lexer "a+b+-c+d") :: Maybe String
	fmap unparse $ expr (lexer "a+-b*c+d") :: Maybe String

-}

module PrecExpr
where
import Char
import Monad

data Expr                     =  Id     String
                              |  Negate Expr
                              |  Plus   Expr Expr
                              |  Minus  Expr Expr
                              |  Times  Expr Expr
                              |  Divide Expr Expr
                              |  Equal  Expr Expr
                                 deriving (Show)

unparse (Id     s)            =  s
unparse (Negate e)            =  "(" ++ "-" ++ unparse e ++ ")"
unparse (Plus   e1 e2)        =  "(" ++ unparse e1 ++ "+" ++ unparse e2 ++ ")"
unparse (Minus  e1 e2)        =  "(" ++ unparse e1 ++ "-" ++ unparse e2 ++ ")"
unparse (Times  e1 e2)        =  "(" ++ unparse e1 ++ "*" ++ unparse e2 ++ ")"
unparse (Divide e1 e2)        =  "(" ++ unparse e1 ++ "/" ++ unparse e2 ++ ")"
unparse (Equal  e1 e2)        =  "(" ++ unparse e1 ++ "=" ++ unparse e2 ++ ")"

type Result                   =  Maybe

%{

Terminal                      =  IDENT {String}
                              |  PLUS
                              |  MINUS
                              |  TIMES
                              |  DIVIDE
                              |  EQUAL
                              |  NEGATE
                              |  LPAR
                              |  RPAR;

nonassoc 4 EQUAL;
left     6 PLUS;
left     6 MINUS;
left     7 TIMES;
left     7 DIVIDE;
nonassoc 8 NEGATE;

Nonterminal                   = *expr {Expr};

expr {Negate e}               :  MINUS, expr {e}, prec NEGATE;
     {Plus   e1 e2}           |  expr {e1}, PLUS,   expr {e2};
     {Minus  e1 e2}           |  expr {e1}, MINUS,  expr {e2};
     {Times  e1 e2}           |  expr {e1}, TIMES,  expr {e2};
     {Divide e1 e2}           |  expr {e1}, DIVIDE, expr {e2};
     {Equal e1 e2}            |  expr {e1}, EQUAL,  expr {e2};
     {e}                      |  LPAR, expr {e}, RPAR;
     {Id s}                   |  IDENT {s};

}%

frown ts                      =  fail "syntax error"

data Terminal                 =  IDENT String | PLUS | MINUS | TIMES | DIVIDE | EQUAL | LPAR | RPAR

lexer                         :: String -> [Terminal]
lexer []                      =  []
lexer ('+' : cs)              =  PLUS   : lexer cs
lexer ('-' : cs)              =  MINUS  : lexer cs
lexer ('*' : cs)              =  TIMES  : lexer cs
lexer ('/' : cs)              =  DIVIDE : lexer cs
lexer ('=' : cs)              =  EQUAL  : lexer cs
lexer ('(' : cs)              =  LPAR   : lexer cs
lexer (')' : cs)              =  RPAR   : lexer cs
lexer (c : cs)
    | isAlpha c               =  let (n, cs') = span isAlphaNum cs
                                 in  IDENT (c : n) : lexer cs'
    | otherwise               =  lexer cs
