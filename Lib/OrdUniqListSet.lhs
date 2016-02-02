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
\section{Sets based on ordered unique lists}
%-------------------------------------------------------------------------------

> module OrdUniqListSet		(  Set,
>                               -- construction
>                                  empty, singleton, union, unionMany,
>                                  add, addMany,
>                               -- destruction
>                                  MinView (..), minview,
>                               -- modification
>                                  intersect, delete, deleteMany, minus,
>				   partition, foldl, foldr,
>                               -- conversion
>                                  toList, fromList, set, list,
>                               -- size
>				   length, genericLength,
>                               -- testing
>				   null, isSingleton, intersecting,
>				   isSubsetOf, elem,
>                               -- substitution
>                                  replaceMaybe, substitute,
>                               -- fixed points
>                                  fixedpoint
>				)
> where

> import Prelude		hiding (  length, null, elem, foldl, foldr  )
> import qualified Prelude
> import qualified OrdUniqList as OUL
> import Prettier               hiding (  empty  )
> import qualified Data.List as List
> import Control.Monad

%-------------------------------------------------------------------------------
\subsection{Type definitions and instance declarations}
%-------------------------------------------------------------------------------

Sets are realized as ordered lists with {\em no} duplicates.

> newtype Set a			=  Set (OUL.OrdUniqList a)
>				   deriving (Eq, Ord)
>
> unSet				:: Set a -> OUL.OrdUniqList a
> unSet (Set s)			=  s

Since the representation of sets is unique we can simply derive \tr{Eq}
and \tr{Ord} instances.

> instance (Ord a, Read a) => Read (Set a) where
>     readsPrec _		=  readSet
>
> instance (Show a) => Show (Set a) where
>     showsPrec _		=  showSet

\tr{readSet} is nearly identical to \tr{readList}. The same holds for
\tr{showSet}.

> readSet		:: (Ord a, Read a) => ReadS (Set a)
> readSet		=  readParen False (\r -> [ pr | ("{", s)  <- lex r,
>                                                        pr        <- readl s ])
>     where readl s	=  [ (empty, t)    | ("}", t)  <- lex s ]
>			++ [ (add x xs, u) | (x, t)    <- reads s,
>                                            (xs, u)   <- readl' t ]
>           readl' s	=  [ (empty, t)    | ("}", t)  <- lex s ]
>			++ [ (add x xs, v) | (",", t)  <- lex s,
>                                            (x, u)    <- reads t,
>                                            (xs, v)   <- readl' u ]
>
> showSet			:: (Show a) => Set a -> ShowS
> showSet (Set [])		=  showString "{}"
> showSet (Set (x : xs))	=  showChar '{' . shows x . showl xs
>     where showl []		=  showChar '}'
>           showl (y : ys)	=  showString ", " . shows y . showl ys

> instance (Pretty a) => Pretty (Set a) where
>       prettyPrec _ (Set as)   =  braces (
>                                    intersperse (char ',' <> nl) (map pretty as))

Yes, \tr{Set} is a functor and a monad.

> instance Functor Set where
>     fmap f (Set s)		=  Set (map f s)
>
> instance Applicative Set where
>     pure = return
>     (<*>) = ap
>
> instance Monad Set where
>     Set s >>= k		=  Set (s >>= (unSet . k))
>     return			=  singleton
>
> --instance MonadPlus Set where
> --    mzero			=  empty

No, \tr{Set} ist {\em not} an instance of \tr{MonadPlus} since
\tr{union} requires the context \tr{Ord a}.

%-------------------------------------------------------------------------------
\subsection{Construction}
%-------------------------------------------------------------------------------

> empty                         :: Set a
> empty				=  Set OUL.empty

> singleton                     :: a -> Set a
> singleton a			=  Set (OUL.singleton a)

> union                         :: Ord a => Set a -> Set a -> Set a
> union (Set s) (Set t)		=  Set (OUL.union s t)

> unionMany                     :: Ord a => [Set a] -> Set a
> unionMany                     =  Set. OUL.unionMany . map unSet

> add                           :: Ord a => a -> Set a -> Set a
> add a (Set s)			=  Set (OUL.add a s)

> addMany			:: Ord a => [a] -> Set a -> Set a
> addMany x (Set s)		=  Set (OUL.addMany x s)

%-------------------------------------------------------------------------------
\subsection{Destruction}
%-------------------------------------------------------------------------------

> data MinView a                =  Empty | Min a (Set a)
>
> minview                       :: Set a -> MinView a
> minview (Set [])              =  Empty
> minview (Set (x : xs))        =  Min x (Set xs)

%-------------------------------------------------------------------------------
\subsection{Modification}
%-------------------------------------------------------------------------------

> intersect			:: Ord a => Set a -> Set a -> Set a
> intersect (Set s) (Set t)	=  Set (OUL.intersect s t)

ToDo: should @delete@ complain if the element is not contained in the
set?

> delete			:: Ord a => a -> Set a -> Set a
> delete a (Set s)		=  Set (OUL.delete a s)

> deleteMany			:: Ord a => [a] -> Set a -> Set a
> deleteMany x (Set s)		=  Set (OUL.deleteMany x s)

> minus				:: Ord a => Set a -> Set a -> Set a
> minus (Set s) (Set t)		=  Set (OUL.minus s t)

> partition			:: (a -> Bool) -> Set a -> (Set a, Set a)
> partition p (Set s)		=  let (t, u) = List.partition p s
>				   in  (Set t, Set u)

> foldl				:: (a -> b -> a) -> a -> Set b -> a
> foldl (*) e (Set s)		=  Prelude.foldl (*) e s

> foldr				:: (a -> b -> b) -> b -> Set a -> b
> foldr (*) e (Set s)		=  Prelude.foldr (*) e s

%-------------------------------------------------------------------------------
\subsection{Conversion}
%-------------------------------------------------------------------------------

> toList, list			:: Set a -> [a]
> toList (Set s)		=  OUL.toList s
> list                          =  toList

> fromList, set			:: Ord a => [a] -> Set a
> fromList x			=  Set (OUL.fromList x)
> set                           =  fromList

%-------------------------------------------------------------------------------
\subsection{Size}
%-------------------------------------------------------------------------------

> length			:: Set a -> Int
> length (Set s)		=  Prelude.length s

> genericLength			:: Integral i => Set a -> i
> genericLength (Set s)		=  List.genericLength s

%-------------------------------------------------------------------------------
\subsection{Testing}
%-------------------------------------------------------------------------------

> null				:: Set a -> Bool
> null (Set s)			=  Prelude.null s

> isSingleton			:: Set a -> Bool
> isSingleton (Set s)		=  OUL.isSingleton s

> intersecting			:: Ord a => Set a -> Set a -> Bool
> intersecting (Set s) (Set t)	=  OUL.intersecting s t

> isSubsetOf			:: Ord a => Set a -> Set a -> Bool
> isSubsetOf (Set s) (Set t)	=  OUL.isSubsetOf s t

> elem				:: Ord a => a -> Set a -> Bool
> a `elem` Set s		=  a `OUL.elem` s

%-------------------------------------------------------------------------------
\subsection{Substitution}
%-------------------------------------------------------------------------------

> replaceMaybe			:: Ord a => (a -> Maybe a) -> Set a -> Set a
> replaceMaybe f (Set s)	=  Set (OUL.replaceMaybe f s)

> substitute			:: Ord a => a -> a -> Set a -> Set a
> substitute a b (Set s)	=  Set (OUL.substitute a b s)

%-------------------------------=  --------------------------------------------
\section{Fixed points}
%-------------------------------=  --------------------------------------------

Fixed point of a set transformer. Well, |fixedpoint f| really computes
the fixed point of |g s = s `union` f s|.

> fixedpoint                    :: Ord a => (Set a -> Set a) -> (Set a -> Set a)
> fixedpoint f s                =  iterate s s
>     where
>     iterate n a
>         | null n'             =  a
>         | otherwise           =  iterate n' (a `union` n')
>         where n'              =  f n `minus` a

