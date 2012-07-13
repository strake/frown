{-

Example for an ambiguous grammar that causes the parser to loop on
backtracking (hidden left recursion).

	frown --debug Loop.g

Try
	
	x "bacc$" :: Maybe ()
	x "bacc$" :: [()]

Using

	frown --debug --trace Loop.g

shows why. Try

	x "bacc$"

-}

module Loop
where
import Monad

instance MonadPlus IO where
    mzero                     =  fail "mzero"
    m `mplus` n               =  putStrLn "** choice" >> m >> putStrLn "** backtrack" >> n

frown ts                      =  putStrLn "*** syntax error" >> return undefined

type Terminal                 =  Char

type Result                   =  IO

{-
frown _                       =  fail "syntax error"
-}

%{

Terminal                      =  'a' | 'b' | 'c' | *'$';
Nonterminal                   =   x  |  y;

x : y, x, 'c';
  | 'a';

y : 'b';
  | ;

}%