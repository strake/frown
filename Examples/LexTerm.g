{-

This example demonstrates the use of a monadic lexer. We use a
continution passing style lexer for reasons of efficiency.

Requires |-98| or |-fglasgow-exts|.

	frown --lexer --expected --signature LexTerm.g

Try

	parse "(2+3)+4" :: IO Expr
	parse "2\n+3\n)+4" :: IO Expr
-}

module LexTerm
where
import Char

data AddOp                    =  Plus  | Minus
                                 deriving (Show)

data Expr                     =  Add Expr AddOp Expr
                              |  Const Int
                                 deriving (Show)

type Result                   =  Lex IO

%{

Terminal                      =  NAT   {Int}   as "numeral"
                              |  ADDOP {AddOp} as "`+' or `-'"
                              |  LPAR          as "`('"
                              |  RPAR          as "`)'"
                              | *EOF           as "<end of input>";

Nonterminal                   = *expr  {Expr}
                              |  term  {Expr};

expr  {Add e1 op e2}          :  expr {e1}, ADDOP {op}, term {e2};
      {e}                     |  term {e};
term  {Const n}               :  NAT {n};
      {e}                     |  LPAR, expr {e}, RPAR;

}%

data Lex m a                  =  Lex { unLex :: forall ans . (a -> [Terminal] -> Int -> m ans)
                                                             -> [Terminal] -> Int -> m ans }

instance Monad (Lex m) where
    return a                  =  Lex (\ cont -> cont a)
    m >>= k                   =  Lex (\ cont -> unLex m (\ a -> unLex (k a) cont))

frown la t                    =  Lex (\ cont inp n ->
                                      fail ("line " ++ show n ++ ": syntax error"
                                            ++ "\nexpected: " ++ concat (intersperse ", " la)
                                            ++ "\nfound   : " ++ unlex (t : inp)))

get                           =  Lex (\ cont inp n ->
                                      case inp of
                                          []           -> cont EOF inp n
                                          SPACE s : ts -> unLex get cont ts (newlines s + n)
                                          t : ts       -> cont t ts n)

data Terminal                 =  SPACE String | NAT Int | ADDOP AddOp | LPAR | RPAR | EOF
                                 deriving (Show)

lexer                         :: String -> [Terminal]
lexer []                      =  [EOF]
lexer ('+' : cs)              =  ADDOP Plus   : lexer cs
lexer ('-' : cs)              =  ADDOP Minus  : lexer cs
lexer ('(' : cs)              =  LPAR : lexer cs
lexer (')' : cs)              =  RPAR : lexer cs
lexer (c : cs)
    | isSpace c               =  let (s, cs') = span isSpace cs
                                 in  SPACE (c : s) : lexer cs'
    | isDigit c               =  let (n, cs') = span isDigit cs
                                 in  NAT (read (c : n)) : lexer cs'
    | otherwise               =  lexer cs

newlines s                    =  sum [ 1 | '\n' <- s ]

unlex []                      =  []
unlex [EOF]                   =  "<end of input>"
unlex (ADDOP Plus  : ts)      =  '+' : unlex ts
unlex (ADDOP Minus : ts)      =  '-' : unlex ts
unlex (LPAR        : ts)      =  '(' : unlex ts
unlex (RPAR        : ts)      =  ')' : unlex ts
unlex (SPACE s     : ts)      =  s      ++ unlex ts
unlex (NAT n       : ts)      =  show n ++ unlex ts

{-
expect EOF                    =  "<end of input>"
expect (ADDOP _)              =  "addition operator"
expect LPAR                   =  "`('"
expect RPAR                   =  "`)'"
expect (NAT _)                =  "numeral"
-}

intersperse		      :: a -> [a] -> [a]
intersperse s []	      =  []
intersperse s (a : as)	      =  a : intersperse1 as
  where intersperse1 []       =  []
        intersperse1 (a : as) =  s : a : intersperse1 as

parse inp                     =  unLex expr (\a _ _ -> return a) (lexer inp) 1