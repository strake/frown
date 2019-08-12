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

> module MergeSort		(  mergeSort, mergeSortBy,
>				   merge, mergeBy, mergeListsBy,
>				   naturalSort, naturalSortBy
>				)
> where

%-------------------------------------------------------------------------------
\section{Mergesort}
%-------------------------------------------------------------------------------

Bottom-up Variant of mergesort.

> mergeSort			:: Ord a => [a] -> [a]
> mergeSort			=  mergeSortBy (<=)

> mergeSortBy			:: (a -> a -> Bool) -> [a] -> [a]
> mergeSortBy (<=)		=  mergeListsBy (<=) . runPhase
>     where

Building ``runs'' of length 2.

>     runPhase []		=  []
>     runPhase [a]		=  [[a]]
>     runPhase (a:b:x)
>         | a <= b		=  [a,b] : runPhase x
>         | otherwise		=  [b,a] : runPhase x

Merging two lists.

> merge				:: Ord a => [a] -> [a] -> [a]
> merge				=  mergeBy (<=)

> mergeBy			:: (a -> a -> Bool) -> [a] -> [a] -> [a]
> mergeBy (_) [] y 		=  y
> mergeBy (_) (a : x) []	=  a : x
> mergeBy (<=) v@(a : x) w@(b : y)
>     | a <= b			=  a : mergeBy (<=) x w
>     | otherwise		=  b : mergeBy (<=) v y

Iteratively merging the runs. Good for its own sake.

> mergeListsBy			:: (a -> a -> Bool) -> [[a]] -> [a]
> mergeListsBy (<=) 		=  mergeLists
>     where
>     mergeLists []		=  []
>     mergeLists [x]		=  x
>     mergeLists (x1:x2:xs)	=  mergeLists (mergeBy (<=) x1 x2:mergePairs xs)
>
>     mergePairs []		=  []
>     mergePairs [x]		=  [x]
>     mergePairs (x1:x2:xs)	=  mergeBy (<=) x1 x2 : mergePairs xs

%-------------------------------------------------------------------------------
\section{Natural mergesort}
%-------------------------------------------------------------------------------

Natural mergesort respect runs of the given list.

> naturalSort			:: Ord a => [a] -> [a]
> naturalSort			=  naturalSortBy (<=)

> naturalSortBy			:: (a -> a -> Bool) -> [a] -> [a]
> naturalSortBy (<=)		=  mergeListsBy (<=) . runPhase
>     where

Splitting into runs. @takeAsc@ takes an ascending prefix.

>     runPhase []		=  [[]]
>     runPhase (a:x)		=  takeAsc [a] x
>
>     takeAsc as []		=  [reverse as]
>     takeAsc as@(a : _) (e : x)
>         | a <= e		=  takeAsc (e : as) x
>         | otherwise		=  takeAscDes as [e] x
>     takeAsc _ _               =  error "takeAsc"
>
>     takeAscDes as ds []	=  [mergeBy (<=) (reverse as) ds]
>     takeAscDes as@(a : _) ds@(d : _) v@(e : x)
>         | a <= e		=  takeAscDes (e : as) ds x
>         | d <= e		=  mergeBy (<=) (reverse as) ds : runPhase v
>         | otherwise		=  takeAscDes as (e : ds) x
>     takeAscDes _ _ _          =  error "takeAscDes"

ToDo: Is @naturalSortBy@ stable?
