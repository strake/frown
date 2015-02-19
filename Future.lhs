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

> module Future			(  module Future
>				)
> where
> import Grammar
> import qualified OrdUniqListSet as Set
> import OrdUniqListSet         (  Set  )
> import qualified OrdUniqListFM as FM
> import OrdUniqListFM          (  FM  )
> -- import Base
> import Prettier

%-------------------------------=  --------------------------------------------
\section{Lookahead information}
%-------------------------------=  --------------------------------------------

> newtype Future                =  Future { unFuture :: FM Symbol Future }
>                                  deriving (Eq, Ord, Show)
>
> fromList                      :: [(Symbol, Future)] -> Future
> fromList ts                   =  Future (FM.fromList ts)
>
> instance Pretty Future where
>     prettyPrec d (Future s)   =  prettyPrec d s
>
> union                         :: Future -> Future -> Future
> union (Future a) (Future b)   =  Future (FM.union_C union a b)
>
> unionMany                     :: [Future] -> Future
> unionMany                     =  Future . FM.unionMany_C union . map unFuture

> prune                         :: Int -> Future -> Future
> prune 0 (Future _ts)          =  fromList []
> prune n (Future ts)           =  fromList [ (a, prune (n - 1) us) | (a, us) <- FM.toList ts ]

> domain                        :: Future -> Set Symbol
> domain (Future f)             =  Set.fromList (map fst (FM.toList f))
>
> lookup                        :: Future -> Symbol -> Maybe Future
> lookup (Future f) a           =  FM.lookup f a
>
> empty                         :: Future -> Bool
> empty (Future f)              =  FM.null f

