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
\section{|Standard.lhs|}
%-------------------------------=  --------------------------------------------

> module Standard               (  generate  )
> where
> import Atom
> import Haskell
> import Grammar               hiding (  prec  )
> import qualified Grammar as G
> import Convert
> import LR0                   hiding (  fromList  )
> import Lookahead
> import Case
> import qualified OrdUniqListSet as Set
> import qualified SearchTree as ST
> import Options
> import Base
> import Generate
> import Data.Char
> import Data.Foldable (  foldMap  )
> import Data.Monoid
> import System.IO
> import Data.Maybe
> import Prelude                hiding (  lookup  )

Characteristics.
%
\begin{description}
\item[required] --
\item[unsupported] --
\end{description}
%
Possible optimization: transitions |s_i >- t -> s_j| such that |s_i|
has only shifts and |t| has no semantic values can be omitted.

%-------------------------------=  --------------------------------------------
\subsection{Helper functions}
%-------------------------------=  --------------------------------------------

> {-

NEU: für die optimierten Reduktionen.

> ntArgsOf v ctx                =  args (pattern v)
>   where
>   args (Case e [(p, e')])     =  Case e [(p, args e')]
>   args e                      =  ctx (map fst (quotesOf e))

> -}

%-------------------------------=  --------------------------------------------
\subsection{Generate Haskell code}
%-------------------------------=  --------------------------------------------

> generate                      :: [Flag] -> [(Symbol, State)] -> GotoTable -> BranchTable -> IO [Decl]
> generate opts entries edges table
>                               =  do verb "* Generating Haskell code ... (--code=standard)"
>                                     return decls
>     where
>     verb                      =  verbose opts

The stack data type.

>     decls                     =  [ DataDecl stack_tcon (
>                                        (unCon empty_con, []) 
>                                        : [ (unCon (con_s_s e), stack_tcon : typesOf v)
>                                          | e@(s, v, s') <- edges ]) ]

The data type of nonterminals. This data type is required if there are
multiple entry points into the parser (that is, multiple start
symbols).

>                               ++ [ Empty
>                                  , DataDecl nonterminal_tcon
>                                       [ (unCon (ntName n), typesOf n) | (n, _) <- entries ] ] 

The parsers for the start symbols.

>                               ++ concat [ Empty
>                                           : [ Sig [unVar (globalNTName n)]
>                                                 (case m_lexName of { Nothing -> [ x_tcon ]; _ -> [] } <->> result_tcon <$> [Tuple (typesOf n)])
>                                             | sigFlag ] 
>                                           ++ [funbind (globalNTName n <$> case m_lexName of { Nothing -> [tr_var]; _ -> [] })
>                                                  (next_n s (empty_con) False <>>=>
>                                                       Fun [ntName n <$> genVars n]
>                                                           (hsReturn <$> [Tuple (genVars n)]))]
>                                         | (n, s) <- entries ]

The |parse_i| functions.

>                               ++ concat [ [ Empty ]
>--                                           , AComment ["state " ++ show (snumber s) ++ reportConflicts cases ++ " "] ]
>                                           ++ [ Sig [unVar (parse_var s)]
>                                                    ([x_tcon, stack_tcon] <->> result_tcon <$> [nonterminal_tcon])
>                                              | sigFlag ]
>--OLD                                           Sig [unVar (parse_var s)] (Con "Monad" <$> [Var "m"] <=>>
>--OLD                                               ([x_tcon, stack_tcon] <->> Var "m" <$> [nonterminal_tcon]))

Problems with supplying the type signatures: for parsers with a
monadic lexer we don't know the type (for instance, `|Lex a|' or
`|Lex m a|' which requires a constraint on `|m|').

>                                           ++ genParse_n s cases
>                                         | (s, cases) <- ST.toList table ]

The |impossible| function (final failure).

>                               ++ (if backtrFlag then
>                                       [ Empty ]
>                                       ++ [ Sig [unVar impossible_var]
>                                                ([x_tcon] <->> result_tcon <$> [nonterminal_tcon])
>                                          | sigFlag ]
>                                       ++ [ funbind (notpossible x_var) (
>                                                hsFail <$> [stringLiteral "\"The `impossible' happened.\""])]
>                                   else
>                                       [])

Options and settings.

>     trFlag                    =  Trace    `elem` opts
>     expFlag                   =  Expected `elem` opts
>     backtrFlag                =  Backtrack `elem` opts
>     sigFlag                   =  Signature False `elem` opts || Signature True `elem` opts
>     Last m_lexName            =  foldMap (\ case { Lexer v -> Last (Just v); _ -> mempty; }) opts
>
>     (x_var, x_tcon)
>       | Just _ <- m_lexName   = (t_var, terminal_tcon)
>       | otherwise             = (ts_var, List [terminal_tcon])

Generate code.

>     genParse_n s (ReduceN as)
>                               =  reduces as Nothing ++ [ impossibleCase s | backtrFlag ]
>     genParse_n s (TokenCase es bs la)
>                               =  concat [ topLevel s e (Just t) | (t, e) <- es ]
>                               ++ catchall s bs la
>     genParse_n _ _            =  impossible "Standard.genParse_n"
>
>     topLevel _s (Shift1 e) _  =  [shift e False]
>     topLevel _s (ReduceN rs) t=  reduces rs t
>     topLevel _s (ShiftReduce e b) _
>                               =  [shift e backtrFlag <||> caseexpr b]
>     topLevel s b t            =  [funbind (parse_n s st_var (genAnoPat t)) (caseexpr b)]
>
>     caseexpr (Shift1 e)       =  shiftRHS e   -- this must be an error-correcting transition
>     caseexpr (ReduceN rs)     =  switch st_var ([ (genStack (stack r), reduceRHS r) | r <- rs ]
>                               ++ [ (anon, notpossible x_var) | backtrFlag ])
>     caseexpr (ReduceReduce rs)=  foldr1 (<|>) [ switch st_var ([ (genStack (stack r), reduceRHS r)]
>                                                                ++ [(anon, frown (Set.empty))]) | r <- rs ] -- TODO: pass set of expected tokens
>     caseexpr (TokenCase es bs la) -- does not work with a monadic lexer
>                               =  switch tr_var ([ ( genNewPat x False, caseexpr t)
>                                                 | (x, t) <- es ]
>                                                 ++ [(anon, catchallRHS bs la)])
>     caseexpr _                =  impossible "Standard.caseexpr"

Code for shift actions. NB `|insert|' shift actions (which are akin to
epsilon transitions) must be treated specially (no input is consumed).

>     shift e@(s, t, _) flag    =  funbind (parse_n s st_var (genNewPat t flag)) (shiftRHS e)
>
>     shiftRHS e@(s, t, s')     =  trace
>                                      (hsPutStrLn <$>
>                                          [stringLiteral ("\"shift " ++ smangle s  ++ " (\"")
>                                           <++> hsShow <$> [fresh t]
>                                           <++> stringLiteral ("\") " ++ smangle s' ++ "\"")])
>                                      (next_n s' (con_s_s e <$> (st_var : genVars t)) (modifier t == Insert))
>
>     next_n s st errCorr
>         | errCorr             =  parse_n s st x_var
>         | Just v <- m_lexName =  var v <>>=> Fun [t'_var] (parse_n s st t'_var)
>         | otherwise           =  parse_n s st tr_var

Generate input pattern for shift action (the as patterns are only
required if the rhs includes reductions).

>     genNewPat v flag
>         | Just _ <- m_lexName =  asPat' t_var (fresh v)
>         | isNewEOF (pattern v)=  asPat' ts_var (asPat tr_var hsNil)
>         | otherwise           =  asPat' ts_var (fresh v <:> tr_var)
>         where asPat' x p
>                   | flag      =  asPat x p
>                   | otherwise =  p

Code for reduce actions.

>     reduces rs x              =  [ reduce r x | r <- rs ]
>
>     reduce r x                =  funbind (parse_n (current r) (genStack (stack r)) (genAnoPat x))
>                                       (reduceRHS r)
>
>     reduceRHS (Reduce _st e@(_s, v, s') _ _ i)
>         | isStart v           =  trace
>                                      (hsPutStrLn <$> [stringLiteral "\"accept\""])
>                                      (evaluate (argsOf v) (\ args -> hsReturn <$> [ntName v <$> args]))
>         | otherwise           =  trace traceReduce
>                                      (evaluate (argsOf v) (\ args ->
>                                          proceed_n s' (con_s_s e <$> (st_var : args))))
>         where
>         traceReduce           =  hsPutStrLn <$> [stringLiteral ("\"reduce by " ++ show i ++ "\"")]
>
>         proceed_n s st'       =  parse_n s st' x_var
>     reduceRHS _               =  impossible "Standard.reduceRHS"

Generate input pattern for reduce action with anonymous variables (to
avoid name capture).

>     genAnoPat Nothing         =  x_var
>     genAnoPat (Just v)
>         | Just _ <- m_lexName =  asPat t_var (anonymous v)
>         | isNewEOF (pattern v)=  asPat ts_var (asPat tr_var hsNil)
>         | otherwise           =  asPat ts_var (anonymous v <:> tr_var)

Generate stack pattern for reduce action.

>     genStack Nil              =  st_var
>     genStack (st :> e@(_, v, _))
>                               =  con_s_s e <$> (genStack st : argsOf v)

Tracing.

>     trace tr e
>         | trFlag              =  tr <>>> e
>         | otherwise           =  e

The catchall case; if we have any |Insert| transitions we use these.

>     catchall s bs la          =  [ funbind (parse_n s st_var x_var) (catchallRHS bs la) ]
>
>     catchallRHS bs la         =  if null bs then frown la else foldr1 (<|>) (map caseexpr bs)
>
>     impossibleCase s          =  funbind (parse_n s st_var x_var) (notpossible x_var)
>
>     frown la
>         | expFlag             =  hsFrown <$> [expected la, x_var]
>         | otherwise           =  hsFrown <$> [x_var]

Possibly generate a backtracking parser.

>     FunBind lhs rhs <||> alt  =  FunBind lhs (rhs <|> alt)
>     _ <||> _                  =  impossible "Standard.<||>"

>     e1 <|> e2
>       | backtrFlag            =  Infix e1 (ident "`mplus`") e2
>       | otherwise             =  e1

Names.

>     parse_n i st ts           =  parse_var i <$> [ts, st]
>     notpossible ts            =  impossible_var <$> [ts]

>     parse_var i               =  wrap_var ("parse_" ++ smangle i)
>     impossible_var            =  wrap_var "impossible"
>     stack_tcon                =  wrap_con "Stack"
>     empty_con                 =  wrap_con "Empty"
>     nonterminal_tcon          =  wrap_con "Nonterminal"
>     st_var                    =  wrap_var "st"
>     ts_var                    =  wrap_var "ts"
>     tr_var                    =  wrap_var "tr"
>     t_var                     =  wrap_var "t"
>     t'_var                    =  wrap_var "t'"
>     con_s_s (s, _v, s')       =  wrap_con ("T_" ++ smangle s ++ "_" ++ smangle s')

>     globalNTName v            =  var (string (name v))
>     ntName v                  =  wrap_con (string (name v))

>     wrap s                    =  prefix opts ++ s ++ suffix opts
>     wrap_var s                =  var (wrap s)
>     wrap_con s                =  con (wrap s)