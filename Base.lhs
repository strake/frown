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

%-------------------------------------------------------------------------------
\section{Auxiliary definitions}
%-------------------------------------------------------------------------------

> module Base                   (  module Base  )
> where
>
> import Prettier
> import Control.Monad                  (  MonadPlus(..)  )
> import System.IO
> import System.Exit

%-------------------------------------------------------------------------------
\subsection{List utilities}
%-------------------------------------------------------------------------------
%{
%format a_(b) = "{" a "}_{" b "}"

> type List a                   =  [a]

> equal                         :: (Eq a) => [a] -> Bool
> equal []                      =  True
> equal [_a]                    =  True
> equal (a1 : as@(a2 : _))      =  a1 == a2 && equal as

The call |breakAfter p [a_(1), ldots, a_(n)]| yields |([a_(1), ldots,
a_(i)], [a_(i+1), ldots , a_(n)])| such that |p (a_(i)) = True| and
|p (a_(j)) = False| for |j < i|.

> breakAfter                    :: (a -> Bool) -> [a] -> ([a], [a])
> breakAfter _p []              =  ([], [])
> breakAfter p (a : as)
>     | p a                     =  ([a], as)
>     | otherwise               =  a <| breakAfter p as

> breaks                        :: ([a] -> Bool) -> [a] -> ([a], [a])
> breaks _p []                  =  ([], [])
> breaks p as@(a : as')
>     | p as                    =  ([], as)
>     | otherwise               =  a <| breaks p as'

> isPrefix                      :: (Eq a) => [a] -> [a] -> Bool
> p `isPrefix` as               =  p == take (length p) as

> {-
> intersperse                   :: a -> [a] -> [a]
> intersperse _s []             =  []
> intersperse s (a : x)         =  a : intersperse1 x
>     where intersperse1 []     =  []
>           intersperse1 (b : y)=  s : b : intersperse1 y
> -}

Required?

> groupBy                       :: (a -> a -> Bool) -> [a] -> [[a]]
> groupBy _p []                 =  []
> groupBy _p [a]                =  [[a]]
> groupBy p (a:b:x) | p a b     =  tack a (groupBy p (b:x))
>                   | otherwise =  [a] : groupBy p (b:x)

> infixr 5 <|  -- same fixity as `|:|'
> (<|)                          :: a -> ([a], b) -> ([a], b)
> a <| (as, b)                  =  (a : as, b)

> tack                          :: a -> [[a]] -> [[a]]
> tack a xs                     =  (a : head xs) : tail xs

%}
%-------------------------------------------------------------------------------
\subsection{Monad utilities}
%-------------------------------------------------------------------------------

> panic                         :: String -> IO a
> panic s                       =  do hPutStrLn stderr ("*** panic: " ++ s)
>                                     exitFailure
> warning                       :: String -> IO ()
> warning s                     =  hPutStrLn stderr ("* warning: " ++ s)
>
> impossible                    :: String -> a
> impossible name               =  error ("The `impossible' happened in \"" ++ name ++ "\".\n"
>                                         ++ "Please, report this as a bug to\n"
>                                         ++ "Ralf Hinze (ralf@cs.uni-bonn.de).")

A simple exception monad.

> data Result a                 =  Fail String | Return a

> instance Monad Result where
>     Fail s   >>= _k           =  Fail s
>     Return a >>= k            =  k a
>     return                    =  Return
>     fail                      =  Fail

> instance MonadPlus Result where
>     mzero                     =  fail ""
>     Fail _s `mplus` m         =  m
>     Return a `mplus` _m       =  Return a

%-------------------------------------------------------------------------------
\subsection{Reverse or snoc lists}
%-------------------------------------------------------------------------------

> infixl 5 :>
> data RevList a                =  Nil | RevList a :> a
>                                  deriving (Show, Eq, Ord)
>
> instance (Pretty a) => Pretty (RevList a) where
>     prettyPrec _ as           =  prettyList (list as)
>
> instance Functor RevList where
>     fmap _f Nil               =  Nil
>     fmap f  (x :> a)          =  fmap f x :> f a
>
> list                          :: RevList a -> List a
> list x                        =  shunt x []
>
> revList                       :: List a -> RevList a
> revList x                     =  revShunt Nil x

> shunt                         :: RevList a -> List a -> List a
> shunt Nil r                   =  r
> shunt (l :> a) r              =  shunt l (a : r)
>
> revShunt                      :: RevList a -> List a -> RevList a
> revShunt l []                 =  l
> revShunt l (a : r)            =  revShunt (l :> a) r
>
> revLength                     :: RevList a -> Int
> revLength Nil                 =  0
> revLength (x :> _a)           =  revLength x + 1
>
> isSuffix                      :: (Eq a) => RevList a -> RevList a -> Bool
> Nil `isSuffix` _y             =  True
> (_x :> _a) `isSuffix` Nil     =  False
> (x :> a) `isSuffix` (y :> b)  =  a == b && x `isSuffix` y

> revTake                       :: Int -> RevList a -> RevList a
> revTake 0 _                   =  Nil
> revTake _ Nil                 =  Nil
> revTake n (as :> a)           =  revTake (n - 1) as :> a

> revDrop                       :: Int -> RevList a -> RevList a
> revDrop 0 as                  =  as
> revDrop _ Nil                 =  Nil
> revDrop n (as :> _a)          =  revDrop (n - 1) as

%-------------------------------------------------------------------------------
\subsection{Formatting text}
%-------------------------------------------------------------------------------

> spaces                        :: Int -> [Char]
> spaces n                      =  replicate n ' '

> cjustifyWith                  :: a -> Int -> [a] -> [a]
> cjustifyWith c n s            =  replicate l c ++ s ++ replicate r c
>     where m                   =  n - length s
>           l                   =  m `div` 2
>           r                   =  m - l

> cjustify                      :: Int -> String -> String
> cjustify                      =  cjustifyWith ' '

> ljustify                      :: Int -> String -> String
> ljustify n s                  =  s ++ spaces (n - length s)

> rjustify                      :: Int -> String -> String
> rjustify n s                  =  spaces (n - length s) ++ s

%-------------------------------------------------------------------------------
\subsection{Miscellaneous}
%-------------------------------------------------------------------------------



> applyWithDefault              :: (a -> Maybe b) -> b -> a -> b
> applyWithDefault f def a      =  case f a of { Nothing -> def; Just v -> v }

> applyWithContinuation         :: (a -> Maybe b) -> (b -> c) -> c -> a -> c
> applyWithContinuation f succ err a
>                               =  case f a of { Nothing -> err; Just v -> succ v }

> equ1                          :: (Eq a) => (a, b) -> (a, b) -> Bool
> equ1 (a, _) (a', _)           =  a == a'
>
> leq1                          :: (Ord a) => (a, b) -> (a, b) -> Bool
> leq1 (a, _) (a', _)           =  a <= a'

> equ2                          :: (Eq b) => (a, b) -> (a, b) -> Bool
> equ2 (_, b) (_, b')           =  b == b'
>
> leq2                          :: (Ord b) => (a, b) -> (a, b) -> Bool
> leq2 (_, b) (_, b')           =  b <= b'

> leqLength                     :: [a] -> [a] -> Bool
> leqLength x y                 =  length x >= length y
