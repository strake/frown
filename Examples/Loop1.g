{-

Example for a LR(0) grammar that causes the parser to loop!

	frown --debug Loop1.g

Try
	
	x "$" :: Maybe ()

Using

	frown --debug --trace Loop1.g

shows why. Try

	x "$"

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

y : ;

}%