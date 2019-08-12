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
\section{Ordered unique lists}
%-------------------------------------------------------------------------------

> module OrdUniqList		(  OrdUniqList,
>                               -- construction
>                                  empty, singleton, union, unionMany,
>				-- construction with combining function
>				   union_C, unionMany_C, add_C, addMany_C,
>                                  add, addMany,
>                               -- modification
>                                  intersect, delete, deleteMany, minus,
>                               -- conversion
>                                  toList, fromList,
>                               -- testing
>                                  isSingleton, intersecting, isSubsetOf,
>                                  elem,
>				-- lookup
>				   lookup, 
>                               -- substitution
>                                  replaceMaybe, substitute,
>				-- *additional functions*
>                                  toSortedList, fromSortedList
>				)
> where

> import Prelude		hiding (  elem, lookup  )
> import MergeSort		(  mergeSort  )
> import Uniq			(  unique  )

%-------------------------------------------------------------------------------
\subsection{Type definitions}
%-------------------------------------------------------------------------------

Ordered lists with \emph{no} duplicates.

> type OrdUniqList a		=  [a]
>
> type OUL a			=  OrdUniqList a	-- abbreviation

%-------------------------------------------------------------------------------
\subsection{Construction}
%-------------------------------------------------------------------------------

> empty                         :: OUL a
> empty				=  []

> singleton                     :: a -> OUL a
> singleton a			=  [a]

@union@ corresponds to \tr{merge}.

> union                         :: (Ord a) => OUL a -> OUL a -> OUL a
> union [] y			=  y
> union x@(_ : _) []		=  x
> union x@(a : x') y@(b : y')	=  case compare a b of
>     LT                        -> a : union x' y
>     EQ                        -> b : union x' y'
>     GT                        -> b : union x  y'

> unionMany                     :: (Ord a) => [OUL a] -> OUL a
> unionMany                     =  foldl union empty

|add| corresponds to |insert|.

> add                           :: (Ord a) => a -> OUL a -> OUL a
> add a []			=  [a]
> add a x@(b : x')		=  case compare a b of
>     LT                        -> a : x
>     EQ                        -> a : x'
>     GT                        -> b : add a x'

> addMany			:: (Ord a) => [a] -> OUL a -> OUL a
> --addMany x s			=  foldr add s x	-- insertion sort
> addMany x s			=  fromList x `union` s	-- merge sort

%-------------------------------------------------------------------------------
\subsection{Construction with combining function}
%-------------------------------------------------------------------------------

The combining function is called as \tr{combine old new} and
\tr{combine left right}, respectively.

> union_C			:: (Ord a) => (a -> a -> a)
>				              -> OUL a -> OUL a -> OUL a
> union_C _combine [] y		=  y
> union_C _combine x@(_ : _) []	=  x
> union_C combine x@(a : x') y@(b : y')
>				=  case compare a b of
>     LT                        -> a : union_C combine x' y
>     EQ                        -> combine a b : union_C combine x' y'
>     GT                       	-> b : union_C combine x  y'

> unionMany_C			:: (Ord a) => (a -> a -> a) -> [OUL a] -> OUL a
> unionMany_C combine		=  foldl (union_C combine) empty

> add_C				:: (Ord a) => (a -> a -> a) -> a -> OUL a -> OUL a
> add_C _combine a []		=  [a]
> add_C combine a x@(b : x')	=  case compare a b of
>     LT                        -> a : x
>     EQ                        -> combine a b : x'
>     GT                        -> b : add_C combine a x'

> addMany_C			:: (Ord a) => (a -> a -> a)
>				              -> OUL a -> [a] -> OUL a
> addMany_C combine x bs	=  union_C combine x (fromList bs)

%-------------------------------------------------------------------------------
\subsection{Modification}
%-------------------------------------------------------------------------------

> intersect			:: (Ord a) => OUL a -> OUL a -> OUL a
> intersect [] _y		=  []
> intersect _x@(_ : _) []	=  []
> intersect x@(a : x') y@(b : y')
>                       	=  case compare a b of
>     LT                        -> intersect x' y
>     EQ                        -> a : intersect x' y'
>     GT                        -> intersect x  y'

ToDo: should @delete@ complain if the element is not contained in the
list?

> delete			:: (Ord a) => a -> OUL a -> OUL a
> delete _a []			=  []
> delete a x@(b : x')		=  case compare a b of
>     LT                        -> x
>     EQ                        -> x'
>     GT                        -> b : delete a x'

> deleteMany			:: (Ord a) => [a] -> OUL a -> OUL a
> deleteMany x s		=  foldr delete s x

> minus				:: (Ord a) => OUL a -> OUL a -> OUL a
> minus [] _y			=  []
> minus x@(_ : _) []		=  x
> minus x@(a : x') y@(b : y')	=  case compare a b of
>     LT                        -> a : minus x' y
>     EQ                        -> minus x' y'
>     GT                        -> minus x  y'

%-------------------------------------------------------------------------------
\subsection{Conversion}
%-------------------------------------------------------------------------------

> toList			:: OUL a -> [a]
> toList			=  id

> fromList			:: (Ord a) => [a] -> OUL a
> fromList			=  fromSortedList . mergeSort

%-------------------------------------------------------------------------------
\subsection{Testing}
%-------------------------------------------------------------------------------

> isSingleton			:: OUL a -> Bool
> isSingleton [_]		=  True
> isSingleton _			=  False

> intersecting			:: (Ord a) => OUL a -> OUL a -> Bool
> intersecting [] _y		=  False
> intersecting (_ : _) []	=  False
> intersecting x@(a : x') y@(b : y')
>                               =  case compare a b of
>     LT                        -> intersecting x' y
>     EQ                        -> True
>     GT                        -> intersecting x  y'

> isSubsetOf			:: (Ord a) => OUL a -> OUL a -> Bool
> isSubsetOf [] _y		=  True
> isSubsetOf (_ : _) []	        =  False
> isSubsetOf x@(a : x') (b : y')=  case compare a b of
>     LT                        -> False
>     EQ                        -> isSubsetOf x' y'
>     GT                        -> isSubsetOf x  y'

NB: we redefine \tr{elem}.

> elem				:: (Ord a) => a -> OUL a -> Bool
> elem _a []			=  False
> elem a (b : x)		=  case compare a b of
>     LT                        -> False
>     EQ                        -> True
>     GT                        -> elem a x

%-------------------------------------------------------------------------------
\subsection{Lookup}
%-------------------------------------------------------------------------------

> lookup			:: (Ord a) => OUL a -> a -> Maybe a
> lookup [] _a			=  Nothing
> lookup (b : x) a		=  case compare a b of
>     LT                        -> Nothing
>     EQ                        -> Just b
>     GT                        -> lookup x a

%-------------------------------------------------------------------------------
\subsection{Substitution}
%-------------------------------------------------------------------------------

@replaceMaybe@ and @substitute@ are quick hacks.

> replaceMaybe			:: (Ord a) => (a -> Maybe a) -> OUL a -> OUL a
> replaceMaybe f x		=  fromList [ b | a <- x, Just b <- [f a] ]

> substitute			:: (Ord a) => a -> a -> OUL a -> OUL a
> substitute a b x
>     | a `elem` x		=  add b (delete a x)
>     | otherwise		=  x

%-------------------------------------------------------------------------------
\subsection{Additional functions}
%-------------------------------------------------------------------------------

> toSortedList			:: OUL a -> [a]
> toSortedList			=  id

> fromSortedList		:: (Eq a) => [a] -> OUL a
> fromSortedList		=  unique

ToDo: \tr{forceDelete} which complains if the element is not contained
in the list. Useful for |substitute|.

> --forceDelete			:: Ord a => a -> OUL a -> Maybe (OUL a)
