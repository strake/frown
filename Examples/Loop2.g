{-

Variation of `Loop1.g' that terminates (because look-ahead information
avoids entering the loop, the look-ahead is empty).

	frown --debug Loop2.g

Try
	
	s "$"

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
Nonterminal                   =   s  |  x  |  y;

s : x;
  | 'a';

x : y, x, 'c';

y : ;

}%