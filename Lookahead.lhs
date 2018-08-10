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

> module Lookahead              (  ActionTable, groupActions, current, ppActionTable
>                               ,  klookahead  )
> where
> import Grammar
> import LR0
> import qualified OrdUniqListSet as Set
> import OrdUniqListSet         (  Set  )
> import qualified SearchTree as FM
> import SearchTree             (  FM  )
> import MergeSort
> import Prettier               hiding (  concat, empty, group  )
> import qualified Prettier as PP
> import Base
> import Options
> import System.IO
> import Data.Maybe
> import Control.Monad                  (  when  )
> import Prelude                hiding (  (<>)  )

%-------------------------------=  --------------------------------------------
\section{Grouping of actions}
%-------------------------------=  --------------------------------------------

Group the actions state-wise.

> type ActionTable              =  FM State [Action]
>
> groupActions                  :: Table -> ActionTable
> groupActions table            =  FM.fromList_C (++) [(current a, [a]) | a <- table ]

> current                       :: Action -> State
> current (Shift (s, _, _))     =  s
> current (Reduce Nil (s, _, _) _ _ _)
>                               =  s
> current (Reduce (_ :> (_, _, s)) _ _ _ _)
>                               =  s

> ppActionTable                 :: ActionTable -> Doc
> ppActionTable table           =  PP.concat [ header ("State " ++ show (snumber s))
>                                              <> pretty acts <> nl <> nl
>                                            | (s, acts) <- FM.toList table ]

%-------------------------------=  --------------------------------------------
\section{Fixed points of finite maps}
%-------------------------------=  --------------------------------------------

Differential fixed point iteration.

> fixedpoint                    :: (Ord a, Show a, Ord v) => [a] -> ((a -> Set v) -> (a -> Set v)) -> ((a -> Set v) -> (a -> Set v))
> fixedpoint dom step start     =  FM.unsafeLookup (FM.fromOrdList (iterate start' start'))
>     where 
>     start'                    =  [ (a, start a) | a <- dom ]
>     step' fm                  =  [ (a, step (FM.unsafeLookup (FM.fromOrdList fm)) a) | a <- dom ]
>
>     iterate n a
>         | null n'             =  a
>         | otherwise           =  iterate n' (a `union` n')
>         where n'              =  step' n `minus` a
>
>     null kvs                  =  and [ Set.null v | (k, v) <- kvs ]
>     kvs1 `union` kvs2         =  [ (k, v1 `Set.union` v2) | ((k, v1), (_, v2)) <- zip kvs1 kvs2 ]
>     kvs1 `minus` kvs2         =  [ (k, v1 `Set.minus` v2) | ((k, v1), (_, v2)) <- zip kvs1 kvs2 ]

Naive fixed point iteration (only used by |nullableOf|).

> naivefixedpoint               :: (Ord a, Show a, Eq v) => [a] -> ((a -> v) -> (a -> v)) -> ((a -> v) -> (a -> v))
> naivefixedpoint dom step start=  FM.unsafeLookup (lfp step' start')
>     where start'              =  FM.fromOrdList [ (a, start a) | a <- dom ]
>           step' fm            =  FM.fromOrdList [ (a, step (FM.unsafeLookup fm) a) | a <- dom ]

> lfp                           :: (Eq a) => (a -> a) -> a -> a
> lfp f a
>    | a == a'                  =  a
>    | otherwise                =  lfp f a'
>    where a'                   =  f a

%-------------------------------=  --------------------------------------------
\section{Nullable symbols}
%-------------------------------=  --------------------------------------------

> nullableOf g                  =  nullable (naivefixedpoint (nonterminals g) step start)
>     where start n             =  False
>
>           step f n            =  or [ nullable f (rrhs r) | r <- productionsOf g n ]
>
>           nullable f []       =  True
>           nullable f (v : vs)
>               | terminal v    =  False
>               | otherwise     =  f v && nullable f vs

%-------------------------------=  --------------------------------------------
\section{LR($\infty$) information}
%-------------------------------=  --------------------------------------------

We partially execute the machine at compile-time to determine look
ahead information.

> klookahead                    :: [Flag] -> Grammar -> GotoTable -> Table -> IO ActionTable
> klookahead opts g gotoTable table'
>                               =  do verb "* Computing k-lookahead information ..."
>                                     verb ("  " ++ show (sum [ 1 | n <- nonterminals g, nullable [n] ]) ++ " nullable nonterminals")
>                                     debug "e-reachable" (pretty [ (e, ereachable e) | e <- gotoTable ])
>                                     debug "reachable" (pretty [ (e, reachable e) | e <- gotoTable ])
>--                                     debug "differential fp iteration" (pretty differeachable)
>                                     return (fmap (map add) table)
>     where

>     pageWidth                 =  head ([ w | Pagewidth w <- opts ] ++ [80]) `max` 40
>     debug s a                 =  when (Debug `elem` opts) (hPutStr stderr t) >> return t
>       where t                 =  cjustifyWith '*' pageWidth (" " ++ s++ " ") ++ "\n\n"
>                               ++ render (Page pageWidth) a ++ "\n\n"

>     verb                      =  verbose opts
>
>     k                         =  lookahead opts
>
>     table                     =  groupActions table'
>
>     lookup s                  =  applyWithDefault (FM.lookup table) [] s
>
>     add a@(Shift _)           =  a
>     add (Reduce st e _ p i)   =  Reduce st e (prune k (lainfo e)) p i
>
>     lainfo                    :: Edge -> Future
>     lainfo e@(_, _, s)        =  fromList [ (t, lainfo e')
>                                           | s /= errorState
>                                           , (_, _, s') <- Set.toList (reachable e)
>                                           , Shift e'@(_, t, _) <- lookup s' ]
>
>     nullable                  =  nullableOf g
>
> {-
>     differeachable            =  fixedpoint gotoTable step start
>       where
>       start e                 =  Set.empty
>
>       step f e@(_, _, s)      =  Set.singleton e `Set.union`
>                                  Set.unionMany [ f (s, v, goto s v)
>                                                | Item _ _ _ (v : _) _ <- toList (items s)
>                                                , nullable [v] ]
> -}
>     ereachable                =  fixedpoint gotoTable step start
>       where
>       start e                 =  Set.empty
>
>       step f e@(_, _, s)      =  Set.singleton e `Set.union`
>                                  Set.unionMany [ f (s, v, goto s v)
>                                                | Item _ _ _ (v : _) _ <- toList (items s)
>                                                , nullable [v] ]
>
>     reachable                 =  fixedpoint gotoTable step start
>       where
>       start e                 =  Set.empty
>
>       step f e@(s0, _, s)     =  ereachable e `Set.union`
>                                  Set.unionMany [ f (s', v, s'')
>                                                | Item _ v (l :> _) r _ <- toList (items s)
>                                                , nullable r
>                                                , s' <- backtrack l s0
>                                                , let s'' = goto s' v
>                                                , s'' /= errorState ]

>     reachableWrong                 =  fixedpoint gotoTable step start --- this is an oversimplification
>       where
>       start e                 =  Set.empty
>
>       step f e@(s0, _, s)     =  Set.singleton e `Set.union`
>                                  Set.unionMany [ f (s, v, goto s v)
>                                                | Item _ _ (_ :> _) (v : _) _ <- toList (items s)
>                                                , nullable [v] ] `Set.union`
>                                  Set.unionMany [ f (s', v, s'')
>                                                | Item _ v (l :> _) [] _ <- toList (items s)
>                                                , s' <- backtrack l s0
>                                                , let s'' = goto s' v
>                                                , s'' /= errorState ]

Copied (more or less) from `LR0.lhs'.

>     backtrack Nil s           =  [ s ]
>     backtrack (vs :> v) s     =  [ x
>                                  | s' <- Set.list (invGoto v s)
>                                  , x <- backtrack vs s' ]
>
>     goto s v                  =  applyWithDefault (FM.lookup fm) errorState (s, v)
>       where fm                =  FM.fromList [ ((si, vi), si') | (si, vi, si') <- gotoTable ]
>
>     invGoto v s'              =  applyWithDefault (FM.lookup fm) Set.empty (v, s')
>       where fm                =  FM.fromList_C Set.union
>                                      [ ((vi, si'), Set.singleton si) | (si, vi, si') <- gotoTable ]

State |0| is the error or trap state and the goto state for start
productions such as |Start# : Start, EOF;|.

> errorState                    :: State
> errorState                    =  State 0 (Set.empty :\/ Set.empty)

%-------------------------------=  --------------------------------------------
\section{OLD: LR($\infty$) information}
%-------------------------------=  --------------------------------------------

We partially execute the machine at compile-time to determine look
ahead information.

This is very subtle: epsilon productions can cause the stack to grow
infinitely. The following grammar serves as an example.

X : Y, X, C;
  | A;
 
Y : B;
  | ;

Therefore we prune the stack to a depth of |k| where k is the number
of lookahead token.

> oldklookahead                    :: [Flag] -> Int -> Table -> IO ActionTable
> oldklookahead opts maxRHS table'
>                               =  do verb "* Computing k-lookahead information ..."
>                                     return (fmap (map add) table)
>     where
>     verb                      =  verbose opts
>
>     k                         =  lookahead opts
>
>     table                     =  groupActions table'
>
>     lookup s                  =  applyWithDefault (FM.lookup table) [] s
>
>     add a@(Shift _)           =  a
>     add (Reduce st e _ p i)   =  Reduce st e (prune k (lainfo 1 (Nil :> e))) p i
>
>     lainfo                    :: Int -> RevList Edge -> Future
>     lainfo j st0              =  --if reachable /= reachable'
>                                  --then error ("LOOKAHEAD ("++ show st0 ++ "):" ++ show reachable ++ "\n" ++ show reachable')
>                                  --else
>                                  fromList [ (t, lainfo (j + 1) (st' :> e))
>                                           | st' <- Set.toList reachable
>                                           , Shift e@(_, t, _) <- lookup (cur st') ]
>--                                           , modifier t == Copy ]
>       where
>       reachable               =  Set.fixedpoint forward (Set.singleton st0)
>
>       forward sts             =  Set.fromList
>                                  [ revTake (10*maxRHS) (nst :> e)   -- prune the stack
>                                  | st <- Set.toList sts
>                                  , Reduce st' e _ _ _ <- lookup (cur st)
>                                  , nst <- matches st st' ]

We have to take |Insert| transitions into account which are akin to
epsilon transitions.

> {-
>                                  `Set.union` Set.fromList
>                                              [ st :> e | st <- Set.toList sts
>                                                        , Shift e@(_, t, _) <- lookup (cur st)
>                                                        , modifier t == Insert ]
> -}

TODO: DANGER of looping??? Inserts are like epsilon transitions.

>       reachable'              =  fmap unConf $ Set.fixedpoint forward' (Set.singleton (Conf st0))
>
>       forward' sts            =  Set.fromList
>                                  [ Conf (nst :> e)
>                                  | Conf st <- Set.toList sts
>                                  , Reduce st' e _ _ _ <- lookup (cur st)
>                                  , nst <- matches st st' ]

> cur                           :: RevList Edge -> State
> cur Nil                       =  impossible "current"
> cur (_ :> (_, _, s))          =  s

> --matches st Nil e             =  [revTake 2 (st :> e)] -- | current st == s ]
> --matches st st' e             =  matches' st st' e

> matches                       :: RevList Edge -> RevList Edge -> [RevList Edge]
> matches Nil _st'              =  [Nil]
> matches st Nil                =  [st]
> matches (st :> t) (st' :> t') =  [ nst | t == t', nst <- matches st st' ]
>--     | t == t'                 =  matches st st'
>--     | otherwise               =  []

> newtype Conf                  =  Conf { unConf :: RevList Edge }
>
> instance Eq Conf where
>     Conf s1 == Conf s2        =  cur s1 == cur s2
>
> instance Ord Conf where
>     compare (Conf s1) (Conf s2)
>                               =  compare (cur s1) (cur s2)
