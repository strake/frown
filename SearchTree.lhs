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

> module SearchTree		(  FM, fromList, fromList_C, fromOrdList, toList
>                               ,  length, lookup, unsafeLookup  )
> where
> import MergeSort		(  mergeSortBy  )
> import Data.Maybe                  (  fromMaybe  )
> import Prelude                hiding (  length, lookup  )
> import qualified Prelude

%-------------------------------------------------------------------------------
\section{Binary search trees}
%-------------------------------------------------------------------------------

> data FM a v                   =  Leaf
>                               |  Node (FM a v) a v (FM a v)
>
> instance Functor (FM a) where
>     fmap _f Leaf              =  Leaf
>     fmap f (Node l a v r)     =  Node (fmap f l) a (f v) (fmap f r)
>
> instance (Eq a, Eq v) => Eq (FM a v) where
>     fm1 == fm2                =  toOrdList fm1 == toOrdList fm2


Construction.

> fromList                      :: (Ord a) => [(a, v)] -> FM a v
> fromList                      =  fromOrdList . mergeSortBy (\ (a1, _) (a2, _) -> a1 <= a2)

> fromOrdList                   :: [(a, v)] -> FM a v
> fromOrdList avs               =  fst (build (Prelude.length avs) avs)
>   where
>   build 0 x                   =  (Leaf, x)
>   build n x                   =  (Node l a v r, z)
>     where m                   =  (n - 1) `div` 2
>           (l, (a, v) : y)     =  build m       x
>           (r, z)              =  build (n - m - 1) y

> fromList_C                    :: (Ord a) => (v -> v -> v) -> [(a, v)] -> FM a v
> fromList_C combine            =  fromOrdList . group . mergeSortBy (\ (a1, _) (a2, _) -> a1 <= a2)
>     where
>     group []			=  []
>     group ((a, v) : x)        =  case group x of
>                                      [] -> [(a, v)]
>                                      y@((a', v') : y')
>                                          | a == a'   -> (a, combine v v') : y'
>                                          | otherwise -> (a, v) : y

> toOrdList                     :: FM a v -> [(a, v)]
> toOrdList t                   =  traverse t []
>   where
>   traverse Leaf x             =  x
>   traverse (Node l a v r) x   =  traverse l ((a, v) : traverse r x)

> toList                        :: FM a v -> [(a, v)]
> toList t                      =  toOrdList t

> length                        :: FM a v -> Int
> length Leaf                   =  0
> length (Node l _a _v r)       =  length l + 1 + length r

Lookup.

> lookup                        :: (Ord a) => FM a v -> a -> Maybe v
> lookup Leaf _x                =  Nothing
> lookup (Node l a v r) x       =  case compare a x of
>                                      LT -> lookup r x
>                                      EQ -> Just v
>                                      GT -> lookup l x


> unsafeLookup                  :: (Ord a, Show a) => FM a v -> a -> v
> unsafeLookup fm a             =  fromMaybe (error ("unsafeLookup: key not found: " ++ show a)) (lookup fm a)