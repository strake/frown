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

> module Case                  (  nexts, Branch(..) -- , caseAnalysis,
>                              ,  reportConflicts, BranchTable, branchLogic  )
> where
> import Grammar               hiding (  prec  )
> import qualified Grammar as G
> import LR0                   hiding (  fromList  )
> import Lookahead
> import Future                hiding (  fromList  )
> import qualified OrdUniqListSet as Set
> import OrdUniqListSet         (  Set  )
> import qualified SearchTree as FM
> import SearchTree             (  FM  )
> import Prettier               hiding (  concat, empty, group, stack  )
> import qualified Prettier as PP
> import MergeSort
> import Base
> import Options
> import Data.Maybe
> import System.IO
> import Control.Monad
> import Data.List                   (  partition  )
> import Prelude                hiding (  lookup, (<>)  )

%-------------------------------=  --------------------------------------------
\section{Back paths}
%-------------------------------=  --------------------------------------------

A trie representing all possible paths back (the trie only contains
reduce actions).

> data Past                     =  Node [Action] [(Edge, Past)]
>                                  deriving (Show)

> extract                       :: Past -> [Action]
> extract (Node as es)          =  as ++ concat [ extract t | (_, t) <- es ]

> isEmpty                       :: Past -> Bool
> isEmpty (Node as es)          =  null as && and [ isEmpty t | (_, t) <- es ]

> collect                       :: Past -> [Action]
> collect (Node as es)          =  as ++ concatMap (collect . snd) es

> past                          :: [Action] -> Past
> past rs                       =  fromOrdList (mergeSort [ (stack r, r) | r <- rs ])
>
> fromOrdList                   :: [(RevList Edge, Action)] -> Past
> fromOrdList []                =  Node [] []
> fromOrdList ((Nil, r) : rs)   =  case fromOrdList rs of
>                                      Node ts es -> Node (r : ts) es
> fromOrdList rs                =  Node [] [ (t, fromOrdList rs') | (t, rs') <- group rs ]

> group                         :: [(RevList Edge, Action)] -> [(Edge, [(RevList Edge, Action)])]
> group []                      =  []
> group ((st1 :> t1, r1) : rs1) =  case group rs1 of
>                                      [] -> [(t1, [(st1, r1)])]
>                                      (t2, rs2) : gs
>                                          | t1 == t2  -> (t2, (st1, r1) : rs2) : gs
>                                          | otherwise -> (t1, [(st1, r1)]) : (t2, rs2) : gs
> group _                       =  impossible "Case.group"
> --group rs                      =  error (show rs)

> lrConflict                    :: Past -> Bool
> lrConflict (Node [] es)       =  or [ lrConflict t | (_, t) <- es]
> lrConflict (Node [_] es)      =  or [ not (isEmpty t) | (_, t) <- es ]
> lrConflict (Node (_ : _ : _) _)
>                               =  True

> lalrConflict                  :: Past -> Bool
> lalrConflict t                =  Set.length (Set.fromList [ pnumber a | a <- as ]) > 1
>     where as                  =  extract t

%-------------------------------=  --------------------------------------------
\section{Branching structures}
%-------------------------------=  --------------------------------------------

> data Branch                   =  Shift1 Edge                  -- no conflict TODO: add lookahead set
>                               |  ReduceN [Action]             -- no conflict
>                               |  ShiftReduce Edge Branch      -- shift/reduce conflict
>                               |  ReduceReduce [Action]        -- reduce/reduce conflict
>                               |  TokenCase [(Symbol, Branch)] [Branch] (Set Symbol)
>			           deriving (Eq, Ord, Show)

NB. |ShiftReduce| and |ReduceReduce| do not appear on the top level as
we always may use one token of lookahead. Likewise, |Shift1| does not
appear on the top level as shift actions use the lookahead. The
default branch in a `|TokenCase|' is either empty, a singleton, or a
list of error correcting shifts.

> instance Pretty Branch where
>     prettyPrec d (Shift1 e)   =  prettyPrec d (Shift e)
>     prettyPrec d (ReduceN as) =  condParens (d > 9)
>                               $  block 4 (string "reductions (non conflicting)"
>                                           </> intersperse nl (map pretty as))
>     prettyPrec d (ShiftReduce e b)
>                               =  condParens (d > 9)
>                               $  block 4 (string "*** shift/reduce conflict"
>                                           </> pretty (Shift e)
>                                           </> pretty b)
>     prettyPrec d (ReduceReduce as)
>                               =  condParens (d > 9)
>                               $  block 4 (string "*** reduce/reduce conflict"
>                                           </> intersperse nl (map pretty as))
>     prettyPrec d (TokenCase es def la)
>                               =  condParens (d > 9)
>                               $  block 4 (string "case"
>                                           </> intersperse nl [ pretty s <+> string " => " <> pretty b | (s, b) <- es ]
>                                           </> string ("default "
>                                                       ++ (if length def > 1 then "*** error-correction conflict " else "")
>                                                       ++ "=> ") <> pretty def
>                                           </> string "lookahead = " <> pretty la)

Order branches by symbol (this is necessary in case patterns are
overlapping).

> tokenCase                     :: [(Symbol, Branch)] -> [Branch] -> Set Symbol -> Branch
> tokenCase                     =  TokenCase . mergeSortBy (\ (v1, _b1) (v2, _b2) -> number v1 <= number v2)

Move identical branches into a default branch.

> optTokenCase                  :: [(Symbol, Branch)] -> [Branch] -> Set Symbol -> Branch
> optTokenCase es def la
>   | null es                   =  tokenCase es def la
>   | null def && length g > 1  =  tokenCase (concat gs) [snd (head g)] la
>   | length def == 1           =  tokenCase es' def la
>   | otherwise                 =  tokenCase es def la
>   where
>   g : gs                      =  mergeSortBy leqLength (groupBy equ2 (mergeSortBy leq2 es))
>   es'                         =  filter (\ (_t, b) -> b /= head def) es

Detection of shift/reduce and reduce/reduce conflicts. Note: we can
also have shift/shift conflicts if there are several `insert'ing
shifts in a default branch.

> shiftReduce                   :: Branch -> Int
> shiftReduce (Shift1 _)        =  0
> shiftReduce (ReduceN _)       =  0
> shiftReduce (ShiftReduce _ _)
>                               =  1
> shiftReduce (ReduceReduce _)
>                               =  0
> shiftReduce (TokenCase es bs _)
>                               =  sum [ shiftReduce t | t <- map snd es ++ bs ]

> reduceReduce                  :: Branch -> Int
> reduceReduce (Shift1 _)       =  0
> reduceReduce (ReduceN _)      =  0
> reduceReduce (ShiftReduce _ b)
>                               =  reduceReduce b
> reduceReduce (ReduceReduce _)
>                               =  1
> reduceReduce (TokenCase es bs _)
>                               =  sum [ reduceReduce t | t <- map snd es ++ bs ]

> insertInsert                  :: Branch -> Int
> insertInsert (Shift1 _)       =  0
> insertInsert (ReduceN _)      =  0
> insertInsert (ShiftReduce _ b)
>                               =  insertInsert b
> insertInsert (ReduceReduce _)
>                               =  0
> insertInsert (TokenCase es bs _)
>                               =  sum [ insertInsert t | t <- map snd es ++ bs ]
>                               +  if length bs > 1 then 1 else 0

> reportConflicts               :: Branch -> String
> reportConflicts b
>     | sr > 0 && rr > 0 && ii > 0
>                               =  ": " ++ show sr ++ " shift/reduce, " ++ show rr ++ " reduce/reduce and " ++ show ii ++ " error-correction conflicts"
>     | sr > 0  && rr > 0       =  ": " ++ show sr ++ " shift/reduce and " ++ show rr ++ " reduce/reduce conflicts"
>     | sr > 0  && ii > 0       =  ": " ++ show sr ++ " shift/reduce and " ++ show ii ++ " error-correction conflicts"
>     | sr > 0                  =  ": " ++ show sr ++ " shift/reduce conflicts"
>     | rr > 0  && ii > 0       =  ": " ++ show rr ++ " reduce/reduce and " ++ show ii ++ " error-correction conflicts"
>     | rr > 0                  =  ": " ++ show rr ++ " reduce/reduce conflicts"
>     | ii > 0                  =  ": " ++ show ii ++ " error-correction conflicts"
>     | otherwise               =  ""
>     where sr                  =  shiftReduce b
>           rr                  =  reduceReduce b
>           ii                  =  insertInsert b

%-------------------------------=  --------------------------------------------
\section{LALR}
%-------------------------------=  --------------------------------------------

> mergeLookaheads               :: [Action] -> [Action]
> mergeLookaheads as            =  map merge as
>     where
>     fm                        =  FM.fromList_C union [ (n, f) | Reduce _ _ f _ n <- as ]
>     merge (Shift e)           =  Shift e
>     merge (Reduce st e _ p n) =  Reduce st e (fromJust (FM.lookup fm n)) p n

%-------------------------------=  --------------------------------------------
\section{Case analysis}
%-------------------------------=  --------------------------------------------

> type BranchTable              =  FM State Branch

> branchLogic                   :: [Flag] -> ActionTable -> IO BranchTable
> branchLogic opts table        =  do verb "* branch logic"
>                                     when (nsr > 0) (warning (show nsr ++ " shift/reduce conflicts"))
>                                     when (nrr > 0) (warning (show nrr ++ " reduce/reduce conflicts"))
>                                     when (nii > 0) (warning (show nii ++ " error-correction conflicts"))
>                                     return branchTable
>     where
>     verb                      =  verbose opts
>     useLALR                   =  Code Stackless `elem` opts || Code GVStack `elem` opts
>     optimize                  =  Optimize `elem` opts
>
>     branchTable               =  fmap caseAnalysis table
>
>     nsr                       =  sum [ shiftReduce b | (s, b) <- FM.toList branchTable ]
>     nrr                       =  sum [ reduceReduce b | (s, b) <- FM.toList branchTable ]
>     nii                       =  sum [ insertInsert b | (s, b) <- FM.toList branchTable ]
>
>     caseAnalysis as
>       | useLALR               =  b
>       | optimize && reduceReduce b == 0
>                               =  b
>       | otherwise             =  topLevelCase opts False as' asErrCorr
>       where (asErrCorr, as')  =  partition isErrCorr as
>             b                 =  topLevelCase opts True (mergeLookaheads as') asErrCorr

> topLevelCase                  :: [Flag] -> Bool -> [Action] -> [Action] -> Branch
> topLevelCase opts lalr        =  topLevel
>     where
>     conflict | lalr           =  lalrConflict
>              | otherwise      =  lrConflict
>
>     optimize                  =  Optimize `elem` opts
>
>     localTokenCase bs def la
>       | optimize              =  optTokenCase bs def la
>       | otherwise             =  tokenCase bs def la
>
>     k                         =  lookahead opts

Toplevel equations. Possibly discriminate on the lookahead token.

>     topLevel                  :: [Action] -> [Action] -> Branch
>     topLevel as asErrCorr
>         | null ss && null asErrCorr && not (conflict (past rs))
>                               =  ReduceN as        -- `|as|' does not contain shifts
>         | otherwise           =  localTokenCase
>                                      [ (t, branch (actions t as)) | t <- Set.toList la, modifier t == Copy ]
>                                      ([ Shift1 e | Shift e <- asErrCorr ]
>                                       ++ [ branch (actions t as) | t <- Set.toList la, modifier t == Insert ]) la
>         where
>         (ss, rs)              =  partition isShift as
>         la                    =  nexts as

One equation for a given token. Use precedences to resolve
shift/reduce conflicts.

>     branch, branch'           :: [Action] -> Branch
>     branch [Shift e]          =  Shift1 e
>     branch rs@(Shift e1@(_, t1, _) : rs')
>         | assoc t1 /= Unspecified && all (isNothing . prec) rs'
>                               =  let ps = map (fromJust . prec) rs'
>                                  in  case (assoc t1, aprec (assoc t1), minimum ps, maximum ps) of
>                                      (_, n1, nmin, nmax)
>                                          | n1 > nmax -> Shift1 e1
>                                          | n1 < nmin -> reduceCase rs'
>                                      (LeftAssoc _, _, nmin, nmax)
>                                          | nmin == nmax -> reduceCase rs'
>                                      (RightAssoc _, _, nmin, nmax)
>                                          | nmin == nmax -> Shift1 e1
>                                      (NonAssoc _, _, nmin, nmax)
>                                          -> TokenCase [] [] Set.empty -- HACK: empty `case' simulates `fail'
>                                      _ -> branch' rs
>     branch rs                 =  branch' rs

Note that there is a good chance that all reductions have the same
precedence: all rules have a common suffix (unless there is an
explicit `|prec|' modifier).  If the suffix contains a terminal, then
the rules have the precedence of the rightmost one.

>     branch' (Shift e : rs')   =  ShiftReduce e (reduceCase rs')
>     branch' rs                =  reduceCase rs

Discriminate on the stack to resolve reduce/reduce conflicts. Possibly
use further lookahead information.

>     reduceCase                :: [Action] -> Branch
>     reduceCase rs             =  caseexpr (k - 1) (past rs)

>     caseexpr                  :: Int -> Past -> Branch
>     caseexpr j t
>         | not (conflict t)    =  ReduceN (collect t)
>         | j == 0              =  ReduceReduce (collect t)
>         | otherwise           =  localTokenCase
>                                      [ (x, caseexpr (j - 1) (actions' x t)) | x <- Set.toList la, modifier x == Copy  ]
>                                      [ caseexpr (j - 1) (actions' x t) | x <- Set.toList la, modifier x == Insert ] la
>         where la              =  nexts' t

> nexts                         :: [Action] -> Set Symbol
> nexts as                      =  Set.unionMany [ la a | a <- as ]
>     where
>     la (Shift (_, t, _))      =  Set.singleton t
>     la (Reduce _ _ f _ _)     =  domain f

> actions                       :: Symbol -> [Action] -> [Action]
> actions x                     =  concatMap extract
>   where
>   extract a@(Shift (_, t, _)) =  [ a | t == x ]
>   extract a@(Reduce {})       =  [ a { future = f } | Just f <- [lookup (future a) x] ]

> isShift                       :: Action -> Bool
> isShift (Shift {})            =  True
> isShift (Reduce {})           =  False

More lookahead.

> nexts'                        :: Past -> Set Symbol
> nexts' (Node es ts)           =  Set.unionMany [ domain (future a) | a <- es ]
>                                  `Set.union` Set.unionMany [ nexts' t | (_, t) <- ts ]

> actions'                      :: Symbol -> Past -> Past
> actions' x (Node es ts)       =  Node [ a{ future = f } | a <- es
>                                                         , Just f <- [lookup (future a) x] ]
>                                       [ (e, actions' x t) | (e, t) <- ts ]
