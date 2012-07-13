


































 module Standard               (  generate  )
 where
 import Atom
 import Haskell
 import Grammar               hiding (  prec  )
 import qualified Grammar as G
 import Convert
 import LR0                   hiding (  fromList  )
 import Lookahead
 import Case
 import qualified OrdUniqListSet as Set
 import qualified SearchTree as ST
 import Options
 import Base
 import Generate
 import Char
 import IO
 import Maybe
 import Prelude                hiding (  lookup  )















 {-



 ntArgsOf v ctx                =  args (pattern v)
   where
   args (Case e [(p, e')])     =  Case e [(p, args e')]
   args e                      =  ctx (map fst (quotesOf e))

 -}





 generate                      :: [Flag] -> [(Symbol, State)] -> GotoTable -> BranchTable -> IO [Decl]
 generate opts entries edges table
                               =  do verb "* Generating Haskell code ... (--code=standard)"
                                     return decls
     where
     verb                      =  verbose opts



     decls                     =  [ DataDecl stack_tcon (
                                        (unCon empty_con, []) 
                                        : [ (unCon (con_s_s e), stack_tcon : typesOf v)
                                          | e@(s, v, s') <- edges ]) ]





                               ++ [ Empty
                                  , DataDecl nonterminal_tcon
                                       [ (unCon (ntName n), typesOf n) | (n, _) <- entries ] ] 



                               ++ concat [ Empty
                                           : [ Sig [unVar (globalNTName n)]
                                                 ([ x_tcon | not lexFlag ] <->> result_tcon <$> [Tuple (typesOf n)])
                                             | sigFlag ] 
                                           ++ [funbind (globalNTName n <$> [tr_var | not lexFlag])
                                                  (next_n s (empty_con) False <>>=>
                                                       Fun [ntName n <$> genVars n]
                                                           (hsReturn <$> [Tuple (genVars n)]))]
                                         | (n, s) <- entries ]



                               ++ concat [ [ Empty ]
--                                           , AComment ["state " ++ show (snumber s) ++ reportConflicts cases ++ " "] ]
                                           ++ [ Sig [unVar (parse_var s)]
                                                    ([x_tcon, stack_tcon] <->> result_tcon <$> [nonterminal_tcon])
                                              | sigFlag ]
--OLD                                           Sig [unVar (parse_var s)] (Con "Monad" <$> [Var "m"] <=>>
--OLD                                               ([x_tcon, stack_tcon] <->> Var "m" <$> [nonterminal_tcon]))





                                           ++ genParse_n s cases
                                         | (s, cases) <- ST.toList table ]



                               ++ (if backtrFlag then
                                       [ Empty ]
                                       ++ [ Sig [unVar impossible_var]
                                                ([x_tcon] <->> result_tcon <$> [nonterminal_tcon])
                                          | sigFlag ]
                                       ++ [ funbind (notpossible x_var) (
                                                hsFail <$> [stringLiteral "\"The `impossible' happened.\""])]
                                   else
                                       [])



     trFlag                    =  Trace    `elem` opts
     lexFlag                   =  Lexer    `elem` opts
     expFlag                   =  Expected `elem` opts
     backtrFlag                =  Backtrack `elem` opts
     sigFlag                   =  Signature False `elem` opts || Signature True `elem` opts

     x_var                     =  if lexFlag then t_var else ts_var
     x_tcon                    =  if lexFlag then terminal_tcon else List [terminal_tcon]



     genParse_n s (ReduceN as)
                               =  reduces as Nothing ++ [ impossibleCase s | backtrFlag ]
     genParse_n s (TokenCase es bs la)
                               =  concat [ topLevel s e (Just t) | (t, e) <- es ]
                               ++ catchall s bs la
     genParse_n _ _            =  impossible "Standard.genParse_n"

     topLevel _s (Shift1 e) _  =  [shift e False]
     topLevel _s (ReduceN rs) t=  reduces rs t
     topLevel _s (ShiftReduce e b) _
                               =  [shift e backtrFlag <||> caseexpr b]
     topLevel s b t            =  [funbind (parse_n s st_var (genAnoPat t)) (caseexpr b)]

     caseexpr (Shift1 e)       =  shiftRHS e   -- this must be an error-correcting transition
     caseexpr (ReduceN rs)     =  switch st_var ([ (genStack (stack r), reduceRHS r) | r <- rs ]
                               ++ [ (anon, notpossible x_var) | backtrFlag ])
     caseexpr (ReduceReduce rs)=  foldr1 (<|>) [ switch st_var ([ (genStack (stack r), reduceRHS r)]
                                                                ++ [(anon, frown (Set.empty))]) | r <- rs ] -- TODO: pass set of expected tokens
     caseexpr (TokenCase es bs la) -- does not work with a monadic lexer
                               =  switch tr_var ([ ( genNewPat x False, caseexpr t)
                                                 | (x, t) <- es ]
                                                 ++ [(anon, catchallRHS bs la)])
     caseexpr _                =  impossible "Standard.caseexpr"




     shift e@(s, t, _) flag    =  funbind (parse_n s st_var (genNewPat t flag)) (shiftRHS e)

     shiftRHS e@(s, t, s')     =  trace
                                      (hsPutStrLn <$>
                                          [stringLiteral ("\"shift " ++ smangle s  ++ " (\"")
                                           <++> hsShow <$> [fresh t]
                                           <++> stringLiteral ("\") " ++ smangle s' ++ "\"")])
                                      (next_n s' (con_s_s e <$> (st_var : genVars t)) (modifier t == Insert))

     next_n s st errCorr
         | errCorr             =  parse_n s st x_var
         | lexFlag             =  hsGet <>>=> Fun [t'_var] (parse_n s st t'_var)
         | otherwise           =  parse_n s st tr_var




     genNewPat v flag
         | lexFlag             =  asPat' t_var (fresh v)
         | isNewEOF (pattern v)=  asPat' ts_var (asPat tr_var hsNil)
         | otherwise           =  asPat' ts_var (fresh v <:> tr_var)
         where asPat' x p
                   | flag      =  asPat x p
                   | otherwise =  p



     reduces rs x              =  [ reduce r x | r <- rs ]

     reduce r x                =  funbind (parse_n (current r) (genStack (stack r)) (genAnoPat x))
                                       (reduceRHS r)

     reduceRHS (Reduce _st e@(_s, v, s') _ _ i)
         | isStart v           =  trace
                                      (hsPutStrLn <$> [stringLiteral "\"accept\""])
                                      (evaluate (argsOf v) (\ args -> hsReturn <$> [ntName v <$> args]))
         | otherwise           =  trace traceReduce
                                      (evaluate (argsOf v) (\ args ->
                                          proceed_n s' (con_s_s e <$> (st_var : args))))
         where
         traceReduce           =  hsPutStrLn <$> [stringLiteral ("\"reduce by " ++ show i ++ "\"")]

         proceed_n s st'       =  parse_n s st' x_var
     reduceRHS _               =  impossible "Standard.reduceRHS"




     genAnoPat Nothing         =  x_var
     genAnoPat (Just v)
         | lexFlag             =  asPat t_var (anonymous v)
         | isNewEOF (pattern v)=  asPat ts_var (asPat tr_var hsNil)
         | otherwise           =  asPat ts_var (anonymous v <:> tr_var)



     genStack Nil              =  st_var
     genStack (st :> e@(_, v, _))
                               =  con_s_s e <$> (genStack st : argsOf v)



     trace tr e
         | trFlag              =  tr <>>> e
         | otherwise           =  e



     catchall s bs la          =  [ funbind (parse_n s st_var x_var) (catchallRHS bs la) ]

     catchallRHS bs la         =  if null bs then frown la else foldr1 (<|>) (map caseexpr bs)

     impossibleCase s          =  funbind (parse_n s st_var x_var) (notpossible x_var)

     frown la
         | expFlag             =  hsFrown <$> [expected la, x_var]
         | otherwise           =  hsFrown <$> [x_var]



     FunBind lhs rhs <||> alt  =  FunBind lhs (rhs <|> alt)
     _ <||> _                  =  impossible "Standard.<||>"

     e1 <|> e2
       | backtrFlag            =  Infix e1 (ident "`mplus`") e2
       | otherwise             =  e1



     parse_n i st ts           =  parse_var i <$> [ts, st]
     notpossible ts            =  impossible_var <$> [ts]

     parse_var i               =  wrap_var ("parse_" ++ smangle i)
     impossible_var            =  wrap_var "impossible"
     stack_tcon                =  wrap_con "Stack"
     empty_con                 =  wrap_con "Empty"
     nonterminal_tcon          =  wrap_con "Nonterminal"
     st_var                    =  wrap_var "st"
     ts_var                    =  wrap_var "ts"
     tr_var                    =  wrap_var "tr"
     t_var                     =  wrap_var "t"
     t'_var                    =  wrap_var "t'"
     con_s_s (s, _v, s')       =  wrap_con ("T_" ++ smangle s ++ "_" ++ smangle s')

     globalNTName v            =  var (string (name v))
     ntName v                  =  wrap_con (string (name v))

     wrap s                    =  prefix opts ++ s ++ suffix opts
     wrap_var s                =  var (wrap s)
     wrap_con s                =  con (wrap s)
