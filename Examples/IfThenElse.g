{-

Solution to the dangling-else problem:

	frown --trace IfThenElse.g

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
Nonterminal                   =  start | stat | matched | unmatched | expr;

start                         :  stat;
stat                          :  matched;
                              |  unmatched;
matched                       :  IF, expr, THEN, matched, ELSE, matched;
                              |  expr, EQU, expr;
unmatched                     :  IF, expr, THEN, stat;
                              |  IF, expr, THEN, matched, ELSE, unmatched;
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
