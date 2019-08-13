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

> module LR0                    (  Item(..), Items(..), itemsToList
>                               ,   lr0automaton, State(..), Edge, GotoTable
>                               ,  Future(..), fromList, union, unionMany, prune
>                               ,  lr0info, Action(..), Table, isErrCorr
>                               ,  backtrack, goto', fixedpoint  )
> where
> import Data.Foldable (toList)
> import Grammar
> import Control.Monad (join)
> import Data.Foldable (find, length)
> import qualified Data.Set as Set
> import Data.Set         (  Set  )
> import qualified Data.Map as Map
> import Data.Map          (  Map  )
> import qualified Data.Map as Map
> import Prettier               hiding (  concat, empty  )
> import Haskell                hiding (  (<$>)  )
> import Future                 hiding (  lookup  )
> import Base
> import Prelude                hiding (  null, (<>)  )
> import System.IO
> import Options

%-------------------------------=  --------------------------------------------
\section{LR(0) items}
%-------------------------------=  --------------------------------------------

An item is a rule with a dot on the right-hand side. We represent the
dotted rhs using a reversed list (a stack) and an ordinary list.

> data Item                     =  Item { inumber :: Int
>                                       , ilhs    :: Symbol
>                                       , istack  :: RevList Symbol
>                                       , iinput  :: List Symbol
>                                       , iprec   :: Prec }
>                                  deriving (Eq, Ord, Show)
>
> instance Pretty Item where
>     prettyPrec d (Item i n l r p)
>                               =  prettyPrec d (Rule i n (shunt l (dot : r)) p)
>         where dot             =  Terminal{ pattern = con ".", modifier = Copy } -- HACK
>
> item                          :: Rule -> Item
> item (Rule i n r p)           =  Item i n Nil r p

Items are either kernel items or predict items. We represent item sets
as the disjoint union of kernel items and predict items. Note that
empty productions are classified as predict items.

> data Items                    =  Set Item :\/ Set Item
>                                  deriving (Show)
>
> instance Pretty Items where
>     prettyPrec d (q :\/ _)    =  prettyPrec d q

> instance Eq Items where
>     (q1 :\/ _) == (q2 :\/ _)  =  q1 == q2
>
> instance Ord Items where
>     compare (q1 :\/ _) (q2 :\/ _)
>                               =  compare q1 q2

Since the kernel items determine the predict items we only compare the
kernel items.

> itemsToList                   :: Items -> [Item]
> itemsToList (q :\/ q')        =  Set.toList (q `Set.union` q')
>
> null                          :: Items -> Bool
> null (q :\/ _)                =  Set.null q

%-------------------------------=  --------------------------------------------
\section{LR(0) automaton}
%-------------------------------=  --------------------------------------------

A state is a numbered item set. The number uniquely determines the
item set.

> data State                    =  State { snumber :: Int
>                                        , items   :: Items }
>                                  deriving (Show)
>
> instance Eq State where
>     s1 == s2                  =  snumber s1 == snumber s2
>
> instance Ord State where
>     compare s1 s2             =  compare (snumber s1) (snumber s2)
>
> instance Pretty State where
>     pretty s                  =  pretty (snumber s)

Calculation of the LR(0) automaton.

> type Edge                     =  (State, Symbol, State)
>
> type GotoTable                =  [Edge]

> lr0automaton                  :: [Flag] -> Grammar -> IO ( [State]
>                                                          , [(Symbol, State)]
>                                                          , GotoTable
>                                                          , Set Symbol)
> lr0automaton opts g           =  do verb "* Computing LR(0) automaton ..."
>                                     verb ("  " ++ show (length states) ++ " states")
>                                     verb ("  " ++ show (length gotoTable) ++ " transitions")

TODO: pretty print not reachable nts.

>                                     verb ("  " ++ show (length reachable) ++ " reachable nonterminals (not reachable: " ++
>                                           show (Set.difference (Set.fromList (nonterminals g)) reachable) ++ ")")
>                                     return (states, initials, gotoTable, reachable)
>     where
>     verb                      =  verbose opts
>
>     closure                   :: Set Item -> Items
>     closure q                 =  q :\/ predict q
>
>     predict                   :: Set Item -> Set Item
>     predict q0                =  fixedpoint step (step q0)
>         where step q          =  Set.fromList [  item p
>                                      |  Item _ _ _ (v : _) _ <- Set.toList q
>                                      ,  nonterminal v
>                                      ,  p <- productionsOf g v ]
>
>     goto                      :: Items -> Map Symbol Items
>     goto q                    =  fmap closure $
>                                  Map.fromListWith Set.union
>                                  [  (v, Set.singleton (Item i n (l :> v) r a))
>                                  |  Item i n l (v : r) a <- itemsToList q ]

Each start symbol gives rise to an initial item set.

>     starts                    =  [ (t, q0 t) | t <- startSymbols g ] --nonterminals g, isStart t ]
>         where q0 s            =  closure $ Set.fromList [ item r | r <- productions g, rlhs r == s]
>
>     itemSets                  =  fixedpoint step (Set.fromList (map snd starts))
>         where step qs         =  Set.fromList [ q'
>                                      | q <- Set.toList qs
>                                      , (_, q') <- Map.toList (goto q) ]

For reasons of effiency and convenience we number the states.

>     fm                        =  [ (q, State n q)
>                                  | (q, n) <- zip (Set.toList itemSets) [1 ..] ]
>
>     states                    =  map snd fm
>
>     initials                  =  [ (t, safeLookup q) | (t, q) <- starts ]
>
>     gotoTable                 =  [ (n, v, safeLookup q')
>                                  | (q, n) <- fm
>                                  , (v, q') <- Map.toList (goto q)
>                                  , not (null q') ] -- we don't list error transitions

>     safeLookup a              =  case lookup a fm of
>                                      Nothing -> error (render (Page 80) (pretty a <> nl <> pretty fm <> nl <> pretty starts))
>                                      Just v  -> v

Determine reachable nonterminals. NB We should use a binary search
tree instead of an ordered list here:

>     reachable                 =  Set.fromList [ v | s <- states, Item _ v _ [] _ <- itemsToList (items s) ]

%-------------------------------=  --------------------------------------------
\section{Shift and reduce table}
%-------------------------------=  --------------------------------------------

> type Table                    =  [Action]
>
> data Action                   =  Shift  { goto    :: Edge }
>                               |  Reduce { stack   :: RevList Edge
>                                         , goto    :: Edge
>                                         , future  :: Future
>                                         , prec    :: Prec
>                                         , pnumber :: Int }
>                                  deriving (Show)
>
> instance Eq Action where
>     Shift e == Shift e'      =  e == e'
>     Reduce st e _f _a i == Reduce st' e' _f' _a' i'
>                              =  (i, st, e) == (i', st', e')
>     _ == _                   =  False
>
> instance Ord Action where
>     compare (Shift e) (Shift e')
>                              =  compare e e'
>     compare (Shift {}) (Reduce {})
>                              =  LT
>     compare (Reduce {}) (Shift {})
>                              =  GT
>     compare (Reduce st e _f _a i) (Reduce st' e' _f' _a' i')
>                              =  compare (i, st, e) (i', st', e')

Note that the reduce actions are (first) ordered by the (length of
the) right-hand side (in particular, the productions must not be
ordered by |pnumber|).

> instance Pretty Action where
>     prettyPrec d (Shift e)    =  condParens (d > 9)
>                               $  block 4 (string "shift " </> pretty e)
>     prettyPrec d (Reduce st e f a i)
>                               =  condParens (d > 9)
>                               $  block 4 (string ("reduce by " ++ show i ++ ":")
>                                           </> intersperse nl (pretty <$> toList st)
>                                           </> string "=>" <+> pretty e
>                                           </> prettyPrec 10 f
>                                           </> prettyPrec 10 a)

> isErrCorr                     :: Action -> Bool
> isErrCorr (Shift (_, v, _))   =  terminal v && modifier v `elem` [Insert, Delete]
> isErrCorr (Reduce {})         =  False

> lr0info                       :: [Flag] -> [State] -> GotoTable -> IO Table
> lr0info opts states gotoTable =  do verb "* Computing actions ..."
>                                     verb ("  " ++ show (length shiftTable) ++ " shift actions")
>                                     verb ("  " ++ show (length reduceTable) ++ " reduce actions")
>                                     return (shiftTable ++ reduceTable)
>     where
>     verb                      =  verbose opts
>
>     shiftTable                =  [ Shift e
>                                  | e@(_, t, _) <- gotoTable
>                                  , terminal t ]
>
>     reduceTable               =  [ Reduce st (s', v, goto' gotoTable s' v) (fromList []) p i
>                                  | n <- states
>                                  , Item i v l [] p <- itemsToList (items n)
>                                  , (st, s') <- backtrack gotoTable l n ]
>

State |0| is the error or trap state and the goto state for start
productions such as |Start# : Start, EOF;|.

> errorState                    :: State
> errorState                    =  State 0 (Set.empty :\/ Set.empty)

> backtrack :: (Ord a, Ord b) => [(b, a, b)] -> RevList a -> b -> [(RevList (b, a, b), b)]
> backtrack gotoTable = go where
>     go Nil s       = [(Nil, s)]
>     go (vs :> v) s = [(st :> (s', v, s), x)
>                      | s' <- Set.toList (invGoto v s)
>                      , (st, x) <- go vs s' ]
>
>     invGoto v s' = applyWithDefault (flip Map.lookup fm) Set.empty (v, s')
>     fm = Map.fromListWith Set.union
>          [ ((vi, si'), Set.singleton si) | (si, vi, si') <- gotoTable ]
> {-# INLINE backtrack #-}
>
> goto' :: (Ord a, Ord b) => [(a, b, State)] -> a -> b -> State
> goto' gotoTable = go where
>   go s v = applyWithDefault (flip Map.lookup fm) errorState (s, v)
>   fm = Map.fromList [ ((si, vi), si') | (si, vi, si') <- gotoTable ]
> {-# INLINE goto' #-}

> fixedpoint                    :: Ord a => (Set a -> Set a) -> (Set a -> Set a)
> fixedpoint f s                =  iterate s s
>     where
>     iterate n a
>         | Set.null n'         =  a
>         | otherwise           =  iterate n' (a `Set.union` n')
>         where n'              =  f n `Set.difference` a
