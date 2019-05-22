%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%   Frown --- An LALR(k) parser generator for Haskell 98                      %
%   Copyright (C) 2001-2005 Ralf Hinze                                        %
%                                                                             %
%   This program is free software; you can redistribute it and/or modify      %
%   it under the terms of the GNU General Public License (version 2) as       %
%   published by the Free Software Foundation.                                %
%                                                                             %
%   This program is distributed in the hope that it will be useful,           %
%   but WITHOUT ANY WARRANTY; without even the implied warranty of            %
%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             %
%   GNU General Public License for more details.                              %
%                                                                             %
%   You should have received a copy of the GNU General Public License         %
%   along with this program; see the file COPYING.  If not, write to          %
%   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,          %
%   Boston, MA 02111-1307, USA.                                               %
%                                                                             %
%   Contact information                                                       %
%   Email:      Ralf Hinze <ralf@cs.uni-bonn.de>                              %
%   Homepage:   http://www.informatik.uni-bonn.de/~ralf/                      %
%   Paper mail: Dr. Ralf Hinze                                                %
%               Institut für Informatik III                                   %
%               Universität Bonn                                              %
%               Römerstraße 164                                               %
%               53117 Bonn, Germany                                           %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

> module Quote ( unquotify )
> where
> import Lexer
> import System.IO
> import Options

Simple facility for a Haskell quote/unquote mechanism:

{ ...
... %{

...... { ... }

... }%
}

> rquote, runquote              :: Token
> rquote                        =  RQuote
> runquote                      =  Special '}'
>
> isLUnquoteOrRQuote            :: Token -> Bool
> isLUnquoteOrRQuote (Special '{')
>                               =  True
> isLUnquoteOrRQuote RQuote     =  True
> isLUnquoteOrRQuote _          =  False

> unquotify			:: [Flag] -> [Token] -> IO [Token]
> unquotify opts ts             =  do verb "* Quote/unquote ..."
>                                     verb ("  " ++ show (length ts') ++ " tokens")
>                                     return ts'
>     where
>     (cs, us)                  =  unquote 0 ts
>     ts'                       =  cs ++ [ Error ("quote/unquote error: incomplete parse"
>                                                 ++ "\n<...> " ++ next 3 (concatMap toString ts))
>                                        | not (null us) ]
>     verb                      =  verbose opts

TODO: what happens if |ts| contains a lexical error?

> unquote                       :: Int -> [Token] -> ([Token], [Token])
> unquote n ts                  =  let (us, vs, n') = lquote n ts
>                                  in  if null vs || head vs == runquote then
>                                          (us, vs)
>                                      else
>                                          let (qs, ws)  = quote (tail vs)
>                                              xs        = literal rquote ws
>                                              (uqs, ys) = unquote n' xs
>                                          in  (us ++ [Quote qs ] ++ uqs, ys)

> quote                         :: [Token] -> ([Token], [Token])
> quote ts                      =  let (us, vs) = break isLUnquoteOrRQuote ts
>                                  in  if null vs || head vs == rquote then
>                                          (us, vs)
>                                      else
>                                          let (qs, ws)  = unquote 0 (tail vs)
>                                              xs        = literal runquote ws
>                                              (uqs, ys) = quote xs
>                                          in  (us ++ [Unquote qs] ++ uqs, ys)

> literal                       :: Token -> [Token] -> [Token]
> literal x []                  =  expected x [EOF]
> literal x (t : ts)
>     | t == x                  =  ts
>     | otherwise               =  expected x ts

> expected                      :: Token -> [Token] -> [Token]
> expected x ts                 =  [Error ("quote/unquote error: expected `" ++ toString x ++ "'"
>                                          ++ "\n<...> " ++ next 3 (concatMap toString ts))]

Breaks at the point where it finds the first left quote or the
first unmatched right unquote.

> lquote                        :: Int -> [Token] -> ([Token], [Token], Int)
> lquote n (LQuote : ts)        =  ([], LQuote : ts, n)
> lquote 0 (Special '}' : ts)   =  ([], Special '}' : ts, 0)
> lquote (n + 1) (Special '}' : ts)
>                               =  Special '}' <| lquote n ts
> lquote n (Special '{' : ts)   =  Special '{' <| lquote (n + 1) ts
> lquote n []                   =  ([], [], n)
> lquote n (t : ts)             =  t <| lquote n ts

> (<|)                          :: a -> ([a], x, y) -> ([a], x, y)
> a <| (as, x, y)               =  (a : as, x, y)

readFile "Expr.g" >>= \ s -> print ((unquotify @@ tokenize) s)

print ((unquotify @@ tokenize) "hello %{ world }%")
