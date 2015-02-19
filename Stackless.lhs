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

%-------------------------------=  --------------------------------------------
\section{|Generate.lhs|}
%-------------------------------=  --------------------------------------------

> module Stackless              (  generate  )
> where
> import Atom
> import Haskell
> import Grammar               hiding (  prec  )
> import qualified Grammar as G
> import Convert
> import LR0                   hiding (  fromList  )
> import Case
> import qualified OrdUniqListSet as Set
> import qualified SearchTree as ST
> import Options
> import Base
> import Generate
> import MergeSort
> import Data.Char
> import Data.Foldable          (  foldMap  )
> import Data.Maybe
> import Data.Monoid
> import System.IO
> import Control.Monad
> import Prelude                hiding (  lookup  )

Characteristics.
%
\begin{description}
\item[required] --
\item[unsupported] |--backtrack| and |--lookahead=k|??
\end{description}

%-------------------------------=  --------------------------------------------
\subsection{Helper functions}
%-------------------------------=  --------------------------------------------

> safeLookup                    :: (Show a, Ord a) => ST.FM a v -> a -> v
> safeLookup fm a               =  fromMaybe (error ("not found: " ++ show a)) (ST.lookup fm a)

> data Branch'                  =  Shift1' Symbol State
>                               |  ReduceN' [Int]
>                               |  ShiftReduce' Symbol State Branch'
>                               |  ReduceReduce' [Int]        -- reduce/reduce conflict
>                               |  TokenCase' [(Symbol, Branch')] [Branch']
>			           deriving (Eq, Ord, Show)

> branch'                       :: Branch -> Branch'
> branch' (Shift1 (_s, v, s'))  =  Shift1' v s'
> branch' (ReduceN as)          =  ReduceN' (map pnumber as)
> branch' (ShiftReduce (_s, v, s') b)
>                               =  ShiftReduce' v s' (branch' b)
> branch' (ReduceReduce as)     =  ReduceReduce' (map pnumber as)
> branch' (TokenCase es bs _la) =  TokenCase' [ (v, branch' b) | (v, b) <- es ] [ branch' b | b <- bs ]

codeLeq (_s1, b1) (_s2, b2)   =  branch' b1 <= branch' b2
codeEqu (_s1, b1) (_s2, b2)   =  branch' b1 == branch' b2

> codeLeq (s1, _b1) (s2, _b2) =  rests s1 <= rests s2
> codeEqu (s1, _b1) (s2, _b2) =  rests s1 == rests s2

> rests s                     =  mergeSort (map iinput (toList (items s)))

%-------------------------------=  --------------------------------------------
\subsection{Generate Haskell code}
%-------------------------------=  --------------------------------------------

> generate                      :: [Flag] -> Grammar -> [(Symbol, State)] -> GotoTable -> BranchTable -> IO [Decl]
> generate opts grammar entries edges table
>                               =  do verb "* Generating Haskell code ... (--code=stackless)"
>                                     when backtrFlag (warning "--backtrack is not supported")
>                                     when trFlag     (warning "--trace is not supported")
>                                     verb ("  identical states: " ++ show [ map (snumber . fst) g | g <- groupedTable, length g > 1 ])
>                                     return decls
>     where
>     verb                      =  verbose opts

The data type of nonterminals (this type is only required if we
generate monomorphic type signatures).

>     decls                     =  (if sigFlag then
>                                       [ TypeDecl parser_type ([x_tcon] <->> 
>                                             result_tcon <$> [if Signature False `elem` opts then nonterminal_tcon else t_var])
>                                       , Empty ]
>                                       ++ (if Signature False `elem` opts then
>                                               [ DataDecl nonterminal_tcon
>                                                   [ (unCon (ntName n), typesOf n) | (n, _) <- entries ] 
>                                               , Empty ]
>                                           else
>                                               [])
>                                   else
>                                       [])

The parsers for the start symbols.

>                               ++ [ funbind (globalNTName n <$> case m_lexName of { Nothing -> [tr_var]; _ -> [] })
>                                       (if Signature False `elem` opts then
>                                            (next_n s [Fun (genVars n ++ [anon]) 
>                                                           (hsReturn <$> [ntName n <$> genVars n])]) <>>=>
>                                                 Fun [ntName n <$> genVars n]
>                                                     (hsReturn <$> [Tuple (genVars n)])
>                                        else
>                                            (next_n s [Fun (genVars n ++ [anon]) 
>                                                           (hsReturn <$> [Tuple (genVars n)])]))
>                                  | (n, s) <- entries ]

The |state_i| functions.

>                               ++ concat [ Empty 
>--                                           : AComment ["state " ++ show (snumber s) ++ reportConflicts cases ++ " "]
>                                           : genState_n s cases
>                                         | (s, cases) <- ST.toList table, not (isIdState s) ]
>--                                         | (s, cases) <- mergeSort (map head groupedTable), not (isIdState s) ]

The |reduce| functions. BUG: if a symbol is unreachable then
reductions must not be generated (cf `Dead.g')

>                               ++ concat [ [ Empty ]
>                                           ++ [ AComment ["# NOINLINE " ++ string (unVar red) ++ " #"] | noinline ]
>                                           ++ [ Sig [unVar red] (((typesOf (rlhs r) <->> parser_type)
>                                                                  : concat [ typesOf v | v <- rrhs r ]) <->> parser_type)
>                                              | sigFlag ]
>                                           ++ [funbind (red <$> ([g_var] ++ concat [ argsOf v | v <- rrhs r ] ++ [ts_var]))
>                                                   (evaluate (argsOf (rlhs r)) (\ args -> g_var <$> (args ++ [ts_var])))]
>                                         | r <- productions grammar
>                                         , not (isPrimed (name (rlhs r)))
>                                         , let red = reduce_var (rnumber r) ]

The |impossible| function (final failure).

>--                               ++ [ Empty
>--                                  , funbind (notpossible x_var) (
>--                                        hsFail <$> [stringLiteral "\"The `impossible' happened.\""])]

Options and settings.

>     k                         =  lookahead opts
>     trFlag                    =  Trace     `elem` opts
>     expFlag                   =  Expected  `elem` opts
>     backtrFlag                =  Backtrack `elem` opts
>     sigFlag                   =  Signature False `elem` opts || Signature True `elem` opts
>     noinline                  =  Noinline `elem` opts
>     optimize                  =  Optimize  `elem` opts
>     Last m_lexName            =  foldMap (\ case { Lexer v -> Last (Just v); _ -> mempty; }) opts
>
>     (x_var, x_tcon)
>       | Just _ <- m_lexName   = (t_var, terminal_tcon)
>       | otherwise             = (ts_var, List [terminal_tcon])

`Identity' states.

>     idStates                  =  ST.fromList [ (s, isId cases) | (s, cases) <- ST.toList table ]
>     isIdState s               =  optimize && safeLookup idStates s
>     isId (ReduceN as)         =  equal (map pnumber as) && stack (head as) /= Nil
>     isId _                    =  False

Note that only kernel items may be `Identity' states (eps productions
are predict items).

`Code-identical' states. TODO: consider also lookahead information.
TODO: order continuation arguments by `|iinput|'.

>     groupedTable              =  groupBy codeEqu (mergeSortBy codeLeq (ST.toList table))
>     codeEquState              =  ST.fromList [ (s, fst (head g)) | g <- groupedTable, (s, _) <- g ]

>     state_n s ks
>       | isIdState s           =  head ks
>       | otherwise             =  state_var s <$> ks
>--       | otherwise             =  state_var (safeLookup codeEquState s) <$> ks

Generate code.

>     genState_n s cases
>                               =  [ Sig [unVar (state_var s)] ([ k_type i | i <- Set.toList q ] <->> parser_type)
>                                  | sigFlag]
>                               ++ [ funbind (state_n s [ k_var i | i <- Set.toList q ] <$> [x_var])
>                                        (local [ funbind (goto_var v <$> (genVars v)) --(argsOf v))
>                                                     (state_n s2 [ kernel i <$> (genVars v) | i <- itemsOf v ])
>                                               | (s1, v, s2) <- edges, s1 == s, nonterminal v ]
>                                            (genBody cases))]
>         where
>         q :\/ _q'             =  items s
>
>         k_type i              =  concat [ typesOf v | v <- iinput i ] <->> parser_type
>
>         genBody (ReduceN as)
>             | equal (map pnumber as)
>                               =  kernel' (head as) <$> [x_var]
>             | otherwise       =  error ("the grammar is not LALR(" ++ show k ++ ")")
>         genBody (TokenCase es bs la)
>                               =  switch x_var ([ (genPat t, genExpr e (genVars t))
>                                                | (t, e) <- es ]
>                                                ++ [(anon, if null bs then
>                                                               frown la
>                                                           else
>                                                               genExpr (head bs) [])])
>         genBody _             =  impossible "Stackless.genBody"
>
>         itemsOf v             =  [ item | item@(Item i n l (v' : r) a) <- LR0.toList (items s), v' == v ]
>
>         kernel i
>             | i `Set.elem` q  =  k_var i
>             | otherwise       =  reduce_var (inumber i) <$> [goto_var (ilhs i)]
>
>         kernel' a
>             | stack a /= Nil  =  k_var' a
>             | otherwise       =  reduce_var (pnumber a) <$> [let (_, n, _) = goto a in goto_var n]
>
>         genExpr (Shift1 (_s, t, s')) vs
>             | modifier t == Insert
>                               =  state_n s' [ kernel i | i <- itemsOf t ] <$> [x_var] -- NB `|t|' must not have semantic values
>             | otherwise       =  next_n s' [ kernel i <$> vs | i <- itemsOf t ]
>         genExpr (ReduceN rs) _vs
>             | equal (map pnumber rs)
>                               =  kernel' (head rs) <$> [x_var]
>             | otherwise       =  error ("the grammar is not LALR(" ++ show k ++ ")")
>         genExpr (ShiftReduce e _b) vs
>                               =  genExpr (Shift1 e) vs -- select shift (for the moment)
>         genExpr (ReduceReduce rs) vs
>                               =  genExpr (ReduceN [head rs]) vs
>         genExpr (TokenCase [] bs la) _vs -- HACK: for empty `case' that simulates `fail'
>                               =  if null bs then
>                                      frown la
>                                  else
>                                      genExpr (head bs) []
>         genExpr (TokenCase es bs la) _vs -- does not work with a monadic lexer
>                               =  switch tr_var ([ (genPat t, genExpr e [])
>                                                 | (t, e) <- es ]
>                                                 ++ [(anon, if null bs then
>                                                                frown la
>                                                            else
>                                                                genExpr (head bs) [])])
>         --impossible "Stackless.genExpr"
>
>     genPat v
>         | Just _ <- m_lexName =  fresh v
>         | isNewEOF (pattern v)=  asPat tr_var hsNil
>         | otherwise           =  fresh v <:> tr_var
>
>     next_n s ks
>         | Just v <- m_lexName =  var v <>>=> state_n s ks
>         | otherwise           =  state_n s ks <$> [tr_var]
>
>     frown la
>         | expFlag             =  hsFrown <$> [expected la, x_var]
>         | otherwise           =  hsFrown <$> [x_var]

Possibly generate a backtracking parser.

> {-
>     FunBind lhs rhs <||> alt  =  FunBind lhs (rhs <|> alt)
>
>     e1 <|> e2
>       | backtrFlag            =  Infix e1 "`mplus`" e2
>       | otherwise             =  e1
> -}

Helper functions.

Names.

>--     notpossible ts            =  impossible_var <$> [ts]

>     state_var s               =  wrap_var ("state_" ++ smangle s)
>     k_var i                   =  wrap_var ("k_" ++ imangle i ++ "_" ++ show (length (list (istack i))))
>     k_var' a                  =  wrap_var ("k_" ++ show (pnumber a) ++ "_" ++ show (length (list (stack a))))
>     goto_var v                =  wrap_var ("goto_" ++ vmangle 1 v)
>     reduce_var i              =  wrap_var ("reduce_" ++ show i)
>     ts_var                    =  wrap_var "ts"
>     tr_var                    =  wrap_var "tr"
>     t_var                     =  wrap_var "t"
>     g_var                     =  wrap_var "g"
>     nonterminal_tcon          =  wrap_con "Nonterminal"
>--     impossible_var            =  wrap_var "impossible"
>     parser_tcon               =  wrap_con "Parser"
>     parser_type
>       | Signature False `elem` opts
>                               =  parser_tcon
>       | otherwise             =  parser_tcon <$> [t_var]

>     globalNTName v            =  var (string (name v))
>     ntName v                  =  wrap_con (string (name v))

>     wrap s                    =  prefix opts ++ s ++ suffix opts
>     wrap_var s                =  var (wrap s)
>     wrap_con s                =  con (wrap s)