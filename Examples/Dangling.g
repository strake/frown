{-

The dangling-else problem: the LR parser has a shift-reduce conflict.

	frown --trace Dangling.g

Try

	start (lexer "if a then if a then a=b else a=c")

-}


module Dangling
where
import Char
import Monad

type Result                   =  []

instance MonadPlus IO where
    mzero                     =  fail "mzero"
    m `mplus` n               =  putStrLn "** choice" >> m >> putStrLn "** backtrack" >> n

--frown ts                      =  putStrLn "*** syntax error" >> return undefined
frown ts                      =  fail "syntax error"

%{

Terminal                      =  IF | THEN | ELSE | EQU | IDENT {String} | *EOF;
Nonterminal                   =  start | stat | expr;

start                         :  stat;
stat                          :  IF, expr, THEN, stat, ELSE, stat;
stat                          :  IF, expr, THEN, stat;
stat                          :  expr, EQU, expr;
expr                          :  IDENT {s};

}%

data Terminal                 =  IF | THEN | ELSE | EQU | IDENT String | EOF
	                         deriving (Show)

lexer                         :: String -> [Terminal]
lexer []                      =  [EOF]
lexer ('=' : cs)              =  EQU : lexer cs
lexer (c : cs)
    | isAlpha c               =  let (n, cs') = span isAlphaNum cs
                                 in  (case (c : n) of
                                          "if"   -> IF
                                          "then" -> THEN
                                          "else" -> ELSE
                                          s      -> IDENT s) : lexer cs'
    | otherwise               =  lexer cs
