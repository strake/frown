{-

DIY tracing. Example taken from the Duponcheel/Swierstra paper.

	frown Example.g

Try

	fmap (\ s -> s "") (s "abba" :: IO ShowS) >>= putStrLn

-}

module Example
where

type Terminal                 =  Char

type Result                   =  IO

%{

Terminal                      =  'a' | 'b';
Nonterminal                   =  s {ShowS} | x {ShowS} | y {ShowS};

s {shift 'a' . y . x . reduce "aYX" 'S'}
  :  'a', y {y}, x {x};
x {shift 'b' . shift 'a' . reduce "ba" 'X'}
  :  'b', 'a';
x {a . reduce "Y" 'X'}
  :  y {a};
x {a . reduce "S" 'X'}
  :  s {a};
y {shift 'b' . reduce "b" 'Y'}
  :  'b';
y {shift 'a' . shift 'b' . reduce "ab" 'Y'}
  :  'a', 'b';

}%

frown ts                      =  fail "syntax error"

nl                            =  showChar '\n'
sp                            =  showChar ' '
shift s                       =  showString "shift " . shows s . nl
reduce ss s                   =  showString "reduce " . shows ss . sp . shows s .nl