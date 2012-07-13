{-

This example demonstrates that parsers can also be used in a local
declaration group--if Haskell only allowed local |data| declarations.
To run this example move in the generated file the |Stack| data type
to the top-level and replace |Stack| by |(Stack e)|.

	frown Local.g

*** Does not work any longer. ***

-}


module Local
where

data AddOp                    =  Plus  | Minus
                                 deriving (Show)
data MulOp                    =  Times | Divide
                                 deriving (Show)

data Algebra e                =  Algebra { add :: e -> AddOp -> e -> e
                                         , mul :: e -> MulOp -> e -> e
                                         , nat :: Int -> e }
                                 

parse                         :: Algebra e -> [Terminal] -> Maybe e
parse alg                     =  expr where
    {
    %{

    Terminal                  =  NAT {Int} | ADDOP {AddOp} | MULOP {MulOp} | LPAR | RPAR;
    Nonterminal               =  Expr {e} | Term {e} | Factor {e};

    Expr {add alg e1 op e2}   :  Expr {e1}, ADDOP {op}, Term {e2};
         {e}                  |  Term {e};
    Term {mul alg e1 op e2}   :  Term {e1}, MULOP {op}, Factor {e2};
         {e}                  |  Factor {e};
    Factor {e}                :  LPAR, Expr {e}, RPAR;
           {nat alg i}        |  NAT {i};

    }%
    frown ts                  =  fail "syntax error"
    }

data Terminal                 =  NAT Int | ADDOP AddOp | MULOP MulOp | LPAR | RPAR

lexer                         :: String -> [Terminal]
lexer []                      =  []
lexer ('+' : cs)              =  ADDOP Plus   : lexer cs
lexer ('-' : cs)              =  ADDOP Minus  : lexer cs
lexer ('*' : cs)              =  MULOP Times  : lexer cs
lexer ('/' : cs)              =  MULOP Divide : lexer cs
lexer ('(' : cs)              =  LPAR : lexer cs
lexer (')' : cs)              =  RPAR : lexer cs
lexer (c : cs)
    | isDigit c               =  let (n, cs') = span isDigit cs
                                 in  NAT (read (c : n)) : lexer cs'
    | otherwise               =  lexer cs

eval                          =  parse (Algebra add mul id) . lexer
    where add v1 Plus   v2    =  v1 + v2
          add v1 Minus  v2    =  v1 - v2
          mul v1 Times  v2    =  v1 * v2
          mul v1 Divide v2    =  v1 `div` v2

data Expr                     =  Add Expr AddOp Expr
                              |  Mul Expr MulOp Expr
                              |  Nat Int
                                 deriving (Show)

tree                          =  parse (Algebra Add Mul Nat) . lexer