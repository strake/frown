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

> module Compact                (  generate  )
> where
> import Atom
> import Haskell
> import Grammar                hiding (  prec  )
> import qualified Grammar as G
> import Convert
> import LR0                    hiding (  fromList  )
> import Lookahead
> import Case
> import qualified OrdUniqListSet as Set
> import OrdUniqListSet         (  Set  )
> import qualified SearchTree as ST
> import Options
> import Base
> import Generate
> import MergeSort
> import Data.Char
> import Data.Foldable          (  foldMap  )
> import Data.Monoid
> import System.IO
> import Data.Maybe
> import Prelude                hiding (  lookup  )

%-------------------------------=  --------------------------------------------
\subsection{Helper functions}
%-------------------------------=  --------------------------------------------

> back                          :: RevList Edge -> State
> back Nil                      =  impossible "Compact.back"
> back (Nil :> (s, _ ,_))       =  s
> back (st :> _)                =  back st

> {-

NEU: für die optimierten Reduktionen.

> ntArgsOf v ctx                =  args (pattern v)
>   where
>   args (Case e [(p, e')])     =  Case e [(p, args e')]
>   args e                      =  ctx (map fst (quotesOf e))
> -}

Extract reductions.

> extract                       :: Branch -> [Action]
> extract (Shift1 _e)           =  []
> extract (ReduceN rs)          =  rs
> extract (ShiftReduce _e b)    =  extract b
> extract (ReduceReduce rs)     =  rs
> extract (TokenCase es bs _la) =  concat [ extract b | b <- map snd es ++ bs ]

> reductions                    :: [Branch] -> [[(Int, Action)]]
> reductions bs                 =  groupBy equ1 (mergeSortBy leq1 rs)
>     where rs                  =  [ (pnumber r, r) | b <- bs, r <- extract b ]

> safeLookup                    :: (Show a, Ord a) => ST.FM a v -> a -> v
> safeLookup fm a               =  fromMaybe (error ("not found: " ++ show a)) (ST.lookup fm a)

Key of a symbol (we cannot use equality below, because terminals with
different modifiers are distinguished).

> data Tree a                   =  Node a [Tree a]
>                                  deriving (Eq, Ord, Show)

> key                           :: Symbol -> Tree Int
> key (Terminal { number = n})  =  Node n []
> key (Nonterminal { number = n, arguments = vs})
>                               =  Node n (map key vs)

%-------------------------------=  --------------------------------------------
\subsection{Generate Haskell code}
%-------------------------------=  --------------------------------------------

> generate                      :: [Flag] -> Grammar -> [(Symbol, State)] 
>                                  -> Set Symbol -> GotoTable -> BranchTable -> IO [Decl]
> generate opts grammar entries reachable edges table
>                               =  do verb "* Generating Haskell code ... (--code=compact)"
> --                                    print (symbols grammar)
>--                                     print reachable
>                                     let sgs = length [ n | (n, b) <- ST.toList singleGotoFM, b ]
>                                     verb ("  " ++ show sgs ++ " singleton gotos (of "
>                                           ++ show (ST.length singleGotoFM) ++ ")")
>                                     let sss = length [ s | (s, b) <- ST.toList shiftOnlyFM, b ]
>                                     verb ("  " ++ show sss ++ " `stateless' states (of "
>                                           ++ show (ST.length shiftOnlyFM) ++ ")")
>                                     return decls
>     where
>     verb                      =  verbose opts

The stack data type. The generation of the stack data type relies on
the fact that terminals and nonterminals are numbered consecutively.

>     decls                     =  [ DataDecl stack_tcon (
>                                        (unCon empty_con, []) 
>                                         : [ (unCon (wrap_con ("T_" ++ s)), state_tcon : stack_tcon : ts)
>                                           | (ts, s) <- stTypes ]
>                                         ++ if optimize then
>                                                [ (unCon (wrap_con ("T'_" ++ s)), stack_tcon : ts)
>                                                | (ts, s) <- stTypes ]
>                                            else
>                                                []) ]
>                               ++ (if ghcFlag then
>                                       []
>                                   else
>                                       let constrs = [ (unCon (s_con s), [])
>                                                     | (s, _) <- ST.toList table
>                                                     , not (stateless s) ]
>                                       in [ Empty
>                                          , DataDecl state_tcon (if null constrs then
>                                                                     [(unCon (wrap_con "Void"), [])]
>                                                                 else constrs) ])

The data type of nonterminals. This is needed if there are multiple
entry points into the parser (that is, multiple start symbols).

>                               ++ [ Empty
>                                  , DataDecl nonterminal_tcon
>                                       [ (unCon (ntName n), typesOf n) | (n, _) <- entries ] ] 
>                               ++ [ Empty ]

The parsers for the start symbols.

>                               ++ concat [ Empty
>                                           : [ Sig [unVar (globalNTName n)]
>                                                 (case m_lexName of { Nothing -> [ x_tcon ]; _ -> []; } <->> result_tcon <$> [Tuple (typesOf n)])
>                                             | sigFlag ] 
>                                           ++ [funbind (globalNTName n <$> case m_lexName of { Nothing -> [tr_var]; _ -> []; })
>                                                  (next_n s (empty_con) False <>>=>
>                                                       Fun [ntName n <$> genVars n]
>                                                           (hsReturn <$> [Tuple (genVars n)]))]
>                                         | (n, s) <- entries ]

The |parse_i| functions.

>                               ++ concat [ Empty 
>--                                           : AComment [" state" ++ (if stateless s then "*" else "") ++ " " ++ show (snumber s) ++ reportConflicts cases ++ " "]

Problems with supplying the type signatures: for parsers with a
monadic lexer we don't know the type (for instance, `|Lex a|' or
`|Lex m a|' which requires a constraint on `|m|').


>                                           : genParse_n s cases
>                                         | (s, cases) <- ST.toList table ]

The |reduce| functions.

>                               ++ concat [ Empty
>                                                : [ funbind (reduce_var p <$>
>                                                      ([x_var] ++ [ s_var | epsilon && not (stateless (let (s, _, _) = goto r' in s)) ] ++ [genStack2 (stack r')]))
>                                                      (reduceRHS' r')
>                                                  | r' <- collapse (map snd prs) ]
>                                                ++ if epsilon || not backtrFlag then
>                                                       []
>                                                   else
>                                                       funbind (reduce_var p <$>
>                                                           ([x_var, st_var]))
>                                                           (notpossible st_var x_var) : []
>                                              | prs <- reductions (map snd (ST.toList table))
>                                              , let (p, r) = head prs, let epsilon = stack r == Nil ] 

The |goto| functions.

>                                       ++ concat [ Empty
>                                                   : [ funbind (goto_var v <$> [s_con s])
>                                                           (parse_var s')
>                                                     | e@(s, v', s') <- edges, v' == v ]
>                                                 | v <- Set.toList reachable, not (singleGoto v) ]

The |impossible| function (final failure).

>                               ++ [ Empty
>                                  , funbind (notpossible st_var x_var) (
>                                        hsFail <$> [stringLiteral "\"The `impossible' happened.\""])]

Options and settings.

>     trFlag                    =  Trace     `elem` opts
>     expFlag                   =  Expected  `elem` opts
>     backtrFlag                =  Backtrack `elem` opts
>     sigFlag                   =  Signature False `elem` opts || Signature True `elem` opts
>     optimize                  =  Optimize  `elem` opts
>     Last m_lexName            =  foldMap (\ case { Lexer v -> Last (Just v); _ -> mempty; }) opts

Group the symbols by type.

>     symbolsByType             =  groupBy equ2 (mergeSortBy leq2 [ (v, typesOf v) | v <- symbols grammar ])
>     stTypes                   =  zip (map (snd . head) symbolsByType) (map show [(1 :: Int) ..])
>     stFM                      =  ST.fromList [ (key v, show i)
>                                              | (i, sx) <- zip [(1 :: Int) ..] symbolsByType, (v, _) <- sx ]
>     lookupStFM v              =  safeLookup stFM (key v)

Determine states with no nonterminal transition.

>     shiftOnlyFM               =  ST.fromList [ (snumber s, and [ singleGoto v
>                                                                | (s1, v, s2) <- edges
>                                                                , s1 == s, nonterminal v ])
>                                              | (s, _) <- ST.toList table ]
>     stateless s               =  optimize && safeLookup shiftOnlyFM (snumber s)

Determine singleton gotos.

>     singleGotoFM              =  ST.fromList [ (n, length [ e | e@(s, v, s') <- edges, v == n ] <= 1)
>                                              | n <- Set.toList reachable ]
>     singleGoto v              =  optimize && safeLookup singleGotoFM v

Generate parser.

>     genParse_n _s (ReduceN as)
>                               =  reduces as Nothing
>     genParse_n s (TokenCase es bs la)
>                               =  concat [ topLevel s e (Just t) | (t, e) <- es ]
>                               ++ catchall s bs la
>     genParse_n _ _            =  impossible "Compact.genParse_n"
>
>     topLevel _s (Shift1 e) _  =  [shift e False]
>     topLevel _s (ReduceN rs) t=  reduces rs t
>     topLevel _s (ShiftReduce e b) _
>                               =  [shift e backtrFlag <||> caseexpr b]
>     topLevel s b t            =  [funbind (parse_n s st_var (genAnoPat t)) (caseexpr b)]
>
>     caseexpr (Shift1 e)       =  shiftRHS e   -- this must be an error-correcting transition
>     caseexpr (ReduceN rs)
>       | equal (map pnumber rs)=  reduceRHS (head rs) True
>       | otherwise             =  switch st_var ([ (genStack1 (stack r), reduceRHS r False) | r <- rs ]
>                               ++ [ (anon, notpossible st_var x_var) | backtrFlag ])
>     caseexpr (ReduceReduce rs)=  foldr1 (<|>) [ switch st_var ([ (genStack1 (stack r), reduceRHS r False)]
>                                                                ++ [(anon, frown (Set.empty))]) | r <- rs ] -- TODO: pass set of expected tokens
>     caseexpr (TokenCase es bs la)
>                               =  switch tr_var ([ ( genNewPat x False, caseexpr t)
>                                                 | (x, t) <- es ]
>                                                 ++ [(anon, catchallRHS bs la)])
>     caseexpr _                =  impossible "Compact.caseexpr"

Code for shift actions. NB `|insert|' shift actions (which are akin to
epsilon transitions) must be treated specially (no input is consumed).

>     shift e@(s, t, _) flag    =  funbind (parse_n s st_var (genNewPat t flag)) (shiftRHS e)
>
>     shiftRHS e@(s, t, s')     =  trace
>                                      (hsPutStrLn <$>
>                                          [stringLiteral ("\"shift " ++ smangle s  ++ " (\"")
>                                           <++> hsShow <$> [fresh t]
>                                           <++> stringLiteral ("\") " ++ smangle s' ++ "\"")])
>                                      (next_n s' (con_s_s e st_var (genVars t)) (modifier t == Insert))
>
>     next_n s st errCorr
>         | errCorr             =  parse_n s st x_var
>         | Just v <- m_lexName =  var v <>>=> Fun [t'_var] (parse_n s st t'_var)
>         | otherwise           =  parse_n s st tr_var

Generate input pattern for shift action (the as patterns are only
needed if the rhs includes reductions).

>     genNewPat v flag
>         | Just _ <- m_lexName =  asPat' t_var (fresh v)
>         | isNewEOF (pattern v)=  asPat' ts_var (asPat tr_var hsNil)
>         | otherwise           =  asPat' ts_var (fresh v <:> tr_var)
>         where asPat' x p
>                   | flag      =  asPat x p
>                   | otherwise =  p

Code for reduce actions.

>     reduces rs x
>       | equal (map pnumber rs)=  [ reduce (head rs) x True ]
>       | otherwise             =  [ reduce r x False | r <- rs ]
>
>     reduce r x True           =  funbind (parse_n (current r) st_var (genAnoPat x))
>                                       (reduceRHS r True)
>     reduce r x False          =  funbind (parse_n (current r) (genStack1 (stack r)) (genAnoPat x))
>                                       (reduceRHS r False)
>
>     reduceRHS (Reduce st (s, _, _) _ _ i) True
>                               =  reduce_var i <$>
>			               ([x_var] ++ [ s_con s | st == Nil && not (stateless s) ] ++ [st_var])
>     reduceRHS (Reduce _ e@(_, v, s') _ _ i) False
>         | isStart v           =  trace
>                                      (hsPutStrLn <$> [stringLiteral "\"accept\""])
>                                      (evaluate (argsOf v) (\ args -> hsReturn <$> [ntName v <$> args]))
>         | otherwise           =  trace traceReduce
>                                      (evaluate (argsOf v) (\args -> 
>                                          proceed_n s' (con_s_s e st_var args)))
>         where
>         traceReduce           =  hsPutStrLn <$> [stringLiteral ("\"reduce by " ++ show i ++ "\"")]
>
>         proceed_n s st'       =  parse_n s st' x_var
>
>     reduceRHS _ _             =  impossible "Compact.reduceRHS"

Separate reduce action.

>     reduceRHS' (Reduce _ (s, v, s') _ _ i)
>         | isStart v           =  trace
>                                      (hsPutStrLn <$> [stringLiteral "\"accept\""])
>                                      (evaluate (argsOf v) (\ args -> hsReturn <$> [ntName v <$> args]))
>         | otherwise           =  trace traceReduce
>                                      (evaluate (argsOf v) (\ args -> 
>                                          proceed_n (x st_var args)))
>         where
>         traceReduce           =  hsPutStrLn <$> [stringLiteral ("\"reduce by " ++ show i ++ "\"")]
>
>         x st vs
>           | stateless s       =  if null vs then st else st_con v <$> (st : vs)
>           | otherwise         =  sts_con v <$> (s_var : st : vs)
>
>         proceed_n st'
>           | singleGoto v      =  parse_n s' st' x_var
>           | otherwise         =  goto_var v <$> [s_var, x_var, st']
>
>     reduceRHS' _              =  impossible "Compact.reduceRHS"

Generate input pattern for reduce action with anonymous variables (to
avoid name capture).

>     genAnoPat Nothing         =  x_var
>     genAnoPat (Just v)
>         | Just _ <- m_lexName =  asPat t_var (anonymous v)
>         | isNewEOF (pattern v)=  asPat ts_var (asPat tr_var hsNil)
>         | otherwise           =  asPat ts_var (anonymous v <:> tr_var)

Tracing.

>     trace tr e
>         | trFlag              =  tr <>>> e
>         | otherwise           =  e

The error case; if we have any |Insert| transitions we use these.

>     catchall s bs la          =  [ funbind (parse_n s st_var x_var) (catchallRHS bs la) ]
>
>     catchallRHS bs la         =  if null bs then frown la else foldr1 (<|>) (map caseexpr bs)
>
>     frown la
>         | expFlag             =  hsFrown <$> [expected la, x_var]
>         | otherwise           =  hsFrown <$> [x_var]
>
>     (x_var, x_tcon)
>         | Just _ <- m_lexName = (t_var, terminal_tcon)
>         | otherwise           = (ts_var, List [terminal_tcon])

Generate stack pattern for reduce action, if flag is |True| then
ignore intermediate states (for shift-only states we use the |St_|
constructors rather than the |StS_| constructors).

>     genStack1 Nil             =  st_var
>     genStack1 (st :> e@(_, v, _))
>                               =  con_s_s e (genStack1 st) (argsOf v)
>
>     genStack2 Nil             =  st_var
>     genStack2 (st :> e@(s, v, _))
>         | stateless s         =  con_s_s e (genStack2 st) (argsOf v)
>         | otherwise           =  sts_con v <$> ((if st == Nil then s_var else anon): genStack2 st : argsOf v)

Stack constructors.

>     con_s_s (s, v, _s') st vs
>         | stateless s         =  if null vs then st else st_con v <$> (st : vs)
>         | otherwise           =  sts_con v <$> (s_con s : st : vs)

Collapse reductions.

>     collapse rs               =  map (fst . head) gs
>         where char r          =  (r, [ stateless s | (s, _, _) <- list (stack r) ])
>               gs              =  groupBy equ2 (mergeSortBy leq2 (map char rs))

Possibly use GHC extensions, that is, unboxed types.

>     ghcFlag                   =  GHC      `elem` opts
>
>     state_tcon | ghcFlag      =  con "Int#"
>                | otherwise    =  wrap_con "State"
>
>     s_con s | ghcFlag         =  stringLiteral (smangle s ++ "#")
>             | otherwise       =  wrap_con ("S_" ++ smangle s)

Possibly generate a backtracking parser.

>     FunBind lhs rhs <||> alt  =  FunBind lhs (rhs <|> alt)
>     _ <||> _                  =  impossible "Compact.<||>"
>
>     e1 <|> e2
>       | backtrFlag            =  Infix e1 (ident "`mplus`") e2
>       | otherwise             =  e1

Names.

>
>     sts_con v                 =  wrap_con ("T_" ++ lookupStFM v)
>     st_con v                  =  wrap_con ("T'_" ++ lookupStFM v)
>
>     parse_n i st ts           =  parse_var i <$> [ts, st]
>     notpossible st ts         =  impossible_var <$> [ts, st]

>     parse_var i               =  wrap_var ("parse_" ++ smangle i)
>     goto_var v                =  wrap_var ("goto_" ++ vmangle 1 v)
>     reduce_var i              =  wrap_var ("reduce_" ++ show i)
>     impossible_var            =  wrap_var "impossible"
>     stack_tcon                =  wrap_con "Stack"
>     empty_con                 =  wrap_con "Empty"
>     nonterminal_tcon          =  wrap_con "Nonterminal"
>     st_var                    =  wrap_var "st"
>     ts_var                    =  wrap_var "ts"
>     tr_var                    =  wrap_var "tr"
>     t_var                     =  wrap_var "t"
>     t'_var                    =  wrap_var "t'"
>     s_var                     =  wrap_var "s"

>     globalNTName v            =  var (string (name v))
>     ntName v                  =  wrap_con (string (name v))

>     wrap s                    =  prefix opts ++ s ++ suffix opts
>     wrap_var s                =  var (wrap s)
>     wrap_con s                =  con (wrap s)