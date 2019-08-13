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

> module GVStack                (  generate  )
> where
> import Atom
> import Haskell
> import Grammar               hiding (  prec  )
> import qualified Grammar as G
> import LR0                   hiding (  fromList  )
> import Case
> import qualified Data.Set as Set
> import qualified Data.Map as Map
> import Options
> import Base
> import Generate
> import System.IO
> import Control.Monad
> import Data.Char
> import Data.Foldable               (  foldMap  )
> import Data.List                   (  maximumBy  )
> import Data.Monoid;
> import Prelude                hiding (  lookup, (<$>)  )

%-------------------------------=  --------------------------------------------
\subsection{Helper functions}
%-------------------------------=  --------------------------------------------

> cmpLength                     :: RevList a -> RevList a -> Ordering
> cmpLength x y                 =  compare (revLength x) (revLength y)

%-------------------------------=  --------------------------------------------
\subsection{Generate Haskell code}
%-------------------------------=  --------------------------------------------

> generate                      :: [Flag] -> Grammar
>                                  -> [(Symbol, State)] -> GotoTable -> BranchTable -> IO [Decl]
> generate opts grammar entries edges table
>                               =  do verb "* Generating Haskell code ... (--code=gvstack)"
>                                     when backtrFlag (warning "--backtrack is not supported")
>                                     when (k > 1)    (warning "--lookahead=k with k > 1 is not supported")
>                                     when trFlag     (warning "--trace is not supported")
>                                     return decls
>     where
>     verb                      =  verbose opts

The data type of nonterminals.

>     decls                     =  [ Empty
>                                  , DataDecl nonterminal_tcon
>                                       [ (unCon (ntName n), typesOf n) | n <- nonterminals grammar ]
>                                  , Empty
>                                  , TypeDecl parser_tcon ([case m_lexName of { Just _ -> terminal_tcon; _ -> List [terminal_tcon]; }]
>                                                          <->> (result_tcon <$> [nonterminal_tcon]))
>                                  , Empty
>                                  , TypeDecl (vstack_tcon <$> [vs_var, v_var])
>                                        (Tuple [Tuple [vs_var, [nonterminal_tcon] <->> parser_tcon], v_var])
>                                  , Empty ]

The parsers for the start symbols.

>                               ++ [ funbind (globalNTName n <$> case m_lexName of { Nothing -> [tr_var]; _ -> [] })
>                                       (next_n s [Tuple []] <>>=>
>                                            Fun [ntName n <$> genVars n]
>                                                (hsReturn <$> [Tuple (genVars n)]))
>                                  | (n, s) <- entries ]

The |state_i| functions.

>                               ++ concat [ Empty
>--                                           : AComment [" state " ++ show (snumber s) ++ reportConflicts cases ++ " "]
>                                           : genState_n s cases
>                                         | (s, cases) <- Map.toList table ]

The |reduce| functions. BUG: if a symbol is unreachable then
reductions must not be generated (cf `Dead.g')

>                               ++ concat [ [ Empty ]
>                                           ++ [ AComment ["# NOINLINE " ++ string (unVar red) ++ " #"] | noinline ]
>                                           ++ [ let cont =  if isStart (rlhs r) then accept_var else g_var in
>                                                funbind (red <$> [genStack (revList (rrhs r)), x_var])
>                                                    (evaluate (argsOf (rlhs r))
>                                                         (\ args -> cont <$> [ntName (rlhs r) <$> args, x_var])) ]
>                                         | r <- productions grammar, let red = reduce_var (rnumber r) ]

The |impossible| function (final failure).

>                               ++ [ Empty
>                                  , funbind (state_var <$> [action_var, goto_var, vs_var, x_var])
>                                        (local [ funbind gs_var (Tuple [vs_var, g_var])
>                                               , funbind (g_var <$> [v_var]) (goto_var <$> [v_var, gs_var])]
>                                             (action_var <$> [case m_lexName of
>                                                                Just _ -> t_var
>                                                                _ -> hsHead <$> [ts_var], gs_var, x_var]))
>                                  , Empty
>                                  , funbind (shift_var <$> [state_var, v_var, vs_var, x_var])
>                                        (case m_lexName of
>                                           Just v -> var v <>>=> (state_var <$> [Tuple [vs_var, v_var]])
>                                           _ -> state_var <$> [Tuple [vs_var, v_var], hsTail <$> [ts_var]])
>                                  , Empty
>                                  , funbind (shift'_var <$> [state_var, v_var, vs_var, x_var])
>                                        (case m_lexName of
>                                           Just _ ->  state_var <$> [Tuple [vs_var, v_var], t_var]
>                                           _ -> state_var <$> [Tuple [vs_var, v_var], ts_var])
>                                  , Empty
>                                  , funbind (accept_var <$> [v_var, anon]) (hsReturn <$> [v_var])
>                                  , Empty
>                                  , funbind (goto_var <$> [state_var, v_var, vs_var])
>                                        (state_var <$> [Tuple [vs_var, v_var]])
>                                  , Empty
>                                  , funbind (error_var <$> ([ la_var | expFlag ] ++ [gs_var, x_var]))
>                                        (hsFrown <$> ([ la_var | expFlag ] ++ [x_var]))
>                                  , Empty
>                                  , funbind (notpossible x_var) (
>                                        hsFail <$> [stringLiteral "\"The `impossible' happened.\""])]

Options and settings.

>     k                         =  lookahead opts
>     trFlag                    =  Trace     `elem` opts
>     expFlag                   =  Expected  `elem` opts
>     backtrFlag                =  Backtrack `elem` opts
>     noinline                  =  Noinline `elem` opts
>     Last m_lexName            =  foldMap (\ case { Lexer v -> Last (Just v); _ -> mempty; }) opts
>
>     genState_n s cases
>                               =  [ Sig [unVar (state_n s)]
>                                        ([stack_type (maximumBy cmpLength [ istack i | i <- Set.toList q ])] <->> parser_tcon)
>                                  , funbind (state_n s)
>                                        (state_var <$> [action_n s, if null gotos then hsUndefined else goto_n s])
>                                  , Empty
>                                  , funbind (action_n s <$> [t_var]) (genBody cases) ]
>                               ++ [ Empty | not (null gotos)]
>                               ++ gotos
>                               ++ [Empty]
>         where
>         q :\/ _q'             =  items s
>
>         gotos                 =  [ funbind (goto_n s <$> [ntName v <$> genVars v])
>                                        (goto_var <$> [state_n s2, Tuple (genVars v)])
>                                  | (s1, v, s2) <- edges, s1 == s, nonterminal v ]
>         genBody (ReduceN rs)
>             | equal (map pnumber rs)
>                               =  reduce_n (head rs)
>             | otherwise       =  error "not LALR" -- TODO: Is this correct? One could use additional lookahead.
>         genBody (TokenCase es bs la)
>                               =  switch t_var ([ (fresh t, genExpr e (genVars t))
>                                                | (t, e) <- es ]
>                                                ++ [(anon, if null bs then
>                                                               frown la
>                                                           else
>                                                               genExpr (head bs) [])])
>         genBody _             =  impossible "GVStack.genBody"
>
>         genExpr (Shift1 (_, t, s')) vs
>                               =  shift <$> [state_n s', Tuple vs]
>             where shift       =  if modifier t == Insert then shift'_var else shift_var
>         genExpr (ReduceN rs) _vs
>             | equal (map pnumber rs)
>                               =  reduce_n (head rs)
>             | otherwise       =  error "not LALR"
>         genExpr (ShiftReduce e _b) vs
>                               =  genExpr (Shift1 e) vs -- select shift (for the moment)
>         genExpr (ReduceReduce rs) vs
>                               =  genExpr (ReduceN [head rs]) vs
>         genExpr _ _           =  impossible "GVStack.genExpr"
>
>     genStack Nil              =  Tuple [anon, g_var]
>     genStack (st :> v)        =  Tuple [Tuple [genStack st, Tuple (argsOf v)], anon]
>
>     stack_type Nil            =  vs_var
>     stack_type (st :> v)      =  vstack_tcon <$> [stack_type st, Tuple (typesOf v)]
>
>     next_n s ks
>         | Just v <- m_lexName =  var v <>>=> state_n' s ks
>         | otherwise           =  state_n' s ks <$> [tr_var]
>         where state_n' i ks'  =  state_n i <$> ks' -- HACK to fit the type
>
>--     catchall' la rhss         =  if null rhss then frown la else foldr1 (<|>) rhss
>
>     frown la
>         | expFlag             =  error_var <$> [expected la]
>         | otherwise           =  error_var
>
>     x_var
>         | Just _ <- m_lexName = t_var
>         | otherwise           = ts_var

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

>     state_n i                 =  wrap_var ("state_"  ++ smangle i)
>     action_n i                =  wrap_var ("action_" ++ smangle i)
>     goto_n i                  =  wrap_var ("goto_"   ++ smangle i)
>     reduce_n a                =  wrap_var ("reduce_" ++ pmangle a)
>     notpossible ts            =  impossible_var <$> [ts]

>     state_var                 =  wrap_var "state"
>     shift_var                 =  wrap_var "shift"
>     shift'_var                =  wrap_var "shift'"
>     action_var                =  wrap_var "action"
>     goto_var                  =  wrap_var "goto"
>     accept_var                =  wrap_var "accept"
>     error_var                 =  wrap_var "error"

>     reduce_var i              =  wrap_var ("reduce_" ++ show i)
>     impossible_var            =  wrap_var "impossible"
>     ts_var                    =  wrap_var "ts"
>     tr_var                    =  wrap_var "tr"
>     t_var                     =  wrap_var "t"
>     g_var                     =  wrap_var "g"
>     gs_var                    =  wrap_var "gs"
>     v_var                     =  wrap_var "v"
>     vs_var                    =  wrap_var "vs"
>     la_var                    =  wrap_var "la"
>     parser_tcon               =  wrap_con "Parser"
>     vstack_tcon               =  wrap_con "VStack"
>     nonterminal_tcon          =  wrap_con "Nonterminal"

>     globalNTName v            =  var (string (name v))
>     ntName v                  =  wrap_con (rename (name v))
>     rename i
>       | isPrimed i            =  string i ++ "'" -- NB. A prime for the new start symbols.
>       | otherwise             =  string i

>     wrap s                    =  prefix opts ++ s ++ suffix opts
>     wrap_var s                =  var (wrap s)
>     wrap_con s                =  con (wrap s)
