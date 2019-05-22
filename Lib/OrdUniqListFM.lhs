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
\section{Finite maps based on ordered unique lists}
%-------------------------------------------------------------------------------

> module OrdUniqListFM		(  FM,
>				-- construction
>				   empty, singleton, union, unionMany,
>				   add, (//),
>				-- construction with combining function
>				   union_C, unionMany_C, add_C, addMany_C,
>				-- modification
>				   intersect, delete, deleteMany, minus,
>				   amap, partition, foldl, foldr,
>				-- conversion
>				   toList, fromList, fromList_C,
>				-- size
>				   length, genericLength,
>				-- testing
>				   null, isSingleton, intersecting, isSubsetOf,
>				-- extraction
>				   elems, indices,
>				-- lookup
>				   (!), lookup, lookupWithDefault,
>				   lookupWithContinuation,
>				-- *additional functions*
>                                  toSortedList, fromSortedList,
>				   --prefixLookup
>				)
> where

> import Prelude		hiding (  length, null, elem, lookup,
>				          foldl, foldr, (<>) )
> import qualified Prelude
> import qualified OrdUniqList as OUL
> import MergeSort		(  mergeSort  )
> import Prettier               hiding (  empty  )
> import qualified Data.List as List
> import Data.Maybe                  (  fromMaybe  )

> infixl 9  !
> infixl 9  //

%-------------------------------------------------------------------------------
\subsection{Type definitions}
%-------------------------------------------------------------------------------

Finite maps are based on ordered lists with {\em no} duplicates. In
order to use the functions defined in \tr{OrdUniqList} we define the
type \tr{Assoc}. Assocations are simply pairs. If they are tested for
equality only the first component (the key) is used.

> newtype Assoc a b		=  Assoc (a, b)
>                                  deriving (Read, Show)
>
> unAssoc			:: Assoc a b -> (a, b)
> unAssoc (Assoc p)		=  p
>
> dummy				:: a -> Assoc a b
> dummy a			=  Assoc (a, undefined)

> instance (Pretty a, Pretty b) => Pretty (Assoc a b) where
>     prettyPrec d (Assoc p)    =  prettyPrec d p

> instance Eq a => Eq (Assoc a b) where
>     Assoc (a, _) == Assoc (b, _)		=  a == b
>
> instance Ord a => Ord (Assoc a b) where
>     compare (Assoc (a, _)) (Assoc (b, _))	=  compare a b

> instance Functor (Assoc a) where
>     fmap f (Assoc (a, v))     =  Assoc (a, f v)

A finite map is realized as an association list.

> newtype FM a b		=  FM (OUL.OrdUniqList (Assoc a b))
>                                  deriving (Show, Read)
>
> unFM				:: FM a b -> OUL.OrdUniqList (Assoc a b)
> unFM (FM f)			=  f

> instance (Pretty a, Pretty b) => Pretty (FM a b) where
>     prettyPrec _d (FM f)      =  braces (
>                                    intersperse (char ',' <> nl) (map pretty f))

> instance (Eq a, Eq b) => Eq (FM a b) where
>     FM f == FM g		=  map unAssoc f == map unAssoc g
>
> instance (Ord a, Ord b) => Ord (FM a b) where
>     compare (FM f) (FM g)	=  compare (map unAssoc f) (map unAssoc g)

> --instances: Read, Show

> instance Functor (FM a) where
>     fmap f (FM g)             =  FM (fmap (fmap f) g)

%-------------------------------------------------------------------------------
\subsection{Construction}
%-------------------------------------------------------------------------------

> empty				:: FM a b
> empty				=  FM OUL.empty

> singleton			:: (a, b) -> FM a b
> singleton b			=  FM (OUL.singleton (Assoc b))

> union				:: Ord a => FM a b -> FM a b -> FM a b
> union (FM s) (FM t)		=  FM (OUL.union s t)

> unionMany			:: Ord a => [FM a b] -> FM a b
> unionMany                     =  FM. OUL.unionMany . map unFM

> add				:: Ord a => (a, b) -> FM a b -> FM a b
> add b (FM f)			=  FM (OUL.add (Assoc b) f)

> (//)				:: Ord a => FM a b -> [(a, b)] -> FM a b
> f // bs			=  f `union` fromList bs

%-------------------------------------------------------------------------------
\subsection{Construction with combining function}
%-------------------------------------------------------------------------------

The combining function is called as \tr{combine old new} and
\tr{combine left right}, respectively.

> union_C			:: Ord a => (b -> b -> b)
>				-> FM a b -> FM a b -> FM a b
> union_C combine (FM f) (FM g)	=  FM (OUL.union_C (map2 combine) f g)

> unionMany_C			:: Ord a => (b -> b -> b) -> [FM a b] -> FM a b
> unionMany_C combine		=  FM . OUL.unionMany_C (map2 combine) . map unFM

> add_C				:: Ord a => (b -> b -> b)
>				-> (a, b) -> FM a b -> FM a b
> add_C combine b (FM f)	=  FM (OUL.add_C (map2 combine) (Assoc b) f)

> addMany_C			:: Ord a => (b -> b -> b)
>				-> [(a, b)] -> FM a b -> FM a b
> addMany_C combine bs (FM f)	=  FM (OUL.addMany_C (map2 combine) f
>				           (map Assoc bs))

> map2                          :: (b1 -> b2 -> b) -> (Assoc a b1 -> Assoc a b2 -> Assoc a b)
> map2 combine (Assoc (a, b1)) (Assoc (_, b2))
>				=  Assoc (a, combine b1 b2)

%-------------------------------------------------------------------------------
\subsection{Modification}
%-------------------------------------------------------------------------------

> intersect			:: Ord a => FM a b -> FM a b -> FM a b
> intersect (FM s) (FM t)	=  FM (OUL.intersect s t)

> delete 			:: Ord a => a -> FM a b -> FM a b
> delete a (FM f)		=  FM (OUL.delete (dummy a) f)

> deleteMany			:: Ord a => FM a b -> [a] -> FM a b
> deleteMany (FM f) as		=  FM (OUL.deleteMany (map dummy as) f)

> minus				:: Ord a => FM a b -> FM a b -> FM a b
> minus (FM s) (FM t)		=  FM (OUL.minus s t)

> amap                          :: ((a, b) -> c) -> (FM a b -> FM a c)
> amap f (FM bs)		=  FM (map (\(Assoc b@(a, _)) ->  Assoc (a, f b)) bs)

> partition			:: ((a, b) -> Bool) -> FM a b -> (FM a b,FM a b)
> partition p (FM f)		=  let (g,h) = List.partition (p . unAssoc) f
>				   in  (FM g, FM h)

> foldl				:: (a -> (b, c) -> a) -> a -> FM b c -> a
> foldl (*) e (FM f)		=  Prelude.foldl (*) e (map unAssoc f)

> foldr				:: ((a, b) -> c -> c) -> c -> FM a b -> c
> foldr (*) e (FM f)		=  Prelude.foldr (*) e (map unAssoc f)

ToDo: should \tr{filter} be redefined?

%-------------------------------------------------------------------------------
\subsection{Conversion}
%-------------------------------------------------------------------------------

> toList			:: FM a b -> [(a, b)]
> toList			=  OUL.toList . map unAssoc . unFM

> fromList			:: (Ord a) => [(a, b)] -> FM a b
> fromList			=  FM . OUL.fromList . map Assoc

The list argument of |fromList_C| need not be functional.

> fromList_C                    :: (Ord a) => (b -> b -> b) -> [(a, b)] -> FM a b
> fromList_C combine            =  FM . group . mergeSort . map Assoc
>     where
>     group []			=  []
>     group (Assoc (a, b) : x)  =  case group x of
>                                      [] -> [Assoc (a, b)]
>                                      y@(Assoc (a', b') : y')
>                                          | a == a'   -> Assoc (a, combine b b') : y'
>                                          | otherwise -> Assoc (a, b) : y

%-------------------------------------------------------------------------------
\subsection{Size}
%-------------------------------------------------------------------------------

> length			:: FM a b -> Int
> length (FM f)			=  Prelude.length f

> genericLength			:: Integral i => FM a b -> i
> genericLength (FM f)		=  List.genericLength f

%-------------------------------------------------------------------------------
\subsection{Testing}
%-------------------------------------------------------------------------------

> null				:: FM a b -> Bool
> null (FM f)			=  Prelude.null f

> isSingleton			:: FM a b -> Bool
> isSingleton (FM f)		=  OUL.isSingleton f

> intersecting			:: Ord a => FM a b -> FM a b -> Bool
> intersecting (FM f) (FM g)	=  OUL.intersecting f g

> isSubsetOf			:: Ord a => FM a b -> FM a b -> Bool
> isSubsetOf (FM f) (FM g)	=  OUL.isSubsetOf f g

%-------------------------------------------------------------------------------
\subsection{Extraction}
%-------------------------------------------------------------------------------

> elems				:: FM a b -> [b]
> elems (FM f)			=  map (snd . unAssoc) f

> indices			:: FM a b -> [a]
> indices (FM f)		=  map (fst . unAssoc) f

%-------------------------------------------------------------------------------
\subsection{Lookup}
%-------------------------------------------------------------------------------

> (!)				:: Ord a => FM a b -> a -> b
> f ! a				=  lookupWithDefault f (error
>				       "OrdUniqListFM.!: elem not found") a

> lookup			:: Ord a => FM a b -> a -> Maybe b
> lookup (FM f)			=  fmap (snd . unAssoc) . OUL.lookup f . dummy

> lookupWithDefault		:: Ord a => FM a b -> b -> a -> b
> lookupWithDefault fm def	=  fromMaybe def . lookup fm

> lookupWithContinuation	:: Ord a => FM a b -> (b -> c) -> c -> a -> c
> lookupWithContinuation fm succ err a
>                       	=  case lookup fm a of { Nothing -> err; Just v -> succ v }

%-------------------------------------------------------------------------------
\subsection{Additional functions}
%-------------------------------------------------------------------------------

> toSortedList			:: FM a b -> [(a, b)]
> toSortedList			=  OUL.toSortedList . map unAssoc . unFM

> fromSortedList		:: Eq a => [(a,b)] -> FM a b
> fromSortedList		=  FM . OUL.fromSortedList . map Assoc

@prefixLookup@ returns the list of all completions of the given list.

 prefixLookup			:: Ord a => FM [a] b -> [a] -> [([a], b)]
 prefixLookup (FM f) s		=  [ b | Assoc b@(a, _)<-f, s `isPrefixOf` a ]
