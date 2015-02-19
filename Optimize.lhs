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

> module Optimize              (  optimize  )
> where
> import Grammar
> import LR0
> import Lookahead
> import Haskell
> import qualified SearchTree as FM
> import Base
> import Maybe                  (  fromMaybe  )

%-------------------------------=  --------------------------------------------
\section{Elimination of reduction by single productions}
%-------------------------------=  --------------------------------------------

We partially execute the machine at compile-time to eliminate
reductions that are caused by single productions.

> optimize                      :: ActionTable -> ActionTable
> optimize table                =  fmap (map opt) table
>     where
>     lookup s                  =  applyWithDefault (FM.lookup table) [] s
>
>     opt a@(Shift _)           =  a
>     opt a@(Reduce{ goto = e })=  a{ goto = fromMaybe e (peval e) }
>
>     peval e@(_, n, s)         =  case [ a | a <- lookup s, match e a ] of
>                                      [Reduce{ stack = Nil :> (_, n', _), goto = e' }]
>                                          -> Just (s0, n1 { pattern = compose n n' n1}, s1)
>                                             where (s0, n1, s1) =  fromMaybe e' (peval e')
>                                      _ ->  Nothing

> match                         :: Edge -> Action -> Bool
> match _e (Shift _)            =  True
> match _e (Reduce{ stack = Nil })
>                               =  True
> match e (Reduce{ stack = _ :> e' })
>                               =  e == e'

> compose                       :: Symbol -> Symbol -> Symbol -> Expr
> compose e p e'                =  Case (Tuple (argsOf e))
>                                      [(Tuple (argsOf p), pattern e')]


> argsOf                        =  map fst . quotesOf . pattern


