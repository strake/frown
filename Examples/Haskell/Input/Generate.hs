



 module Generate              (  generate  )
 where
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
 import MergeSort
 -- import Pretty                 hiding (  concat, stack, group  )
 import Char
 import Maybe
 import List                   (  partition  )
 import Prelude                hiding (  lookup  )





 back                          :: RevList Edge -> State
 back (Nil :> (s, _ ,_))       =  s
 back (st :> _)                =  back st



 isCopy (Shift (_, Terminal { modifier = Copy}, _))
                               =  True
 isCopy (Reduce _ _ _ _ _)     =  True
 isCopy _                      =  False

 argsOf                        =  map fst . quotesOf . pattern
 typesOf                       =  map snd . quotesOf . pattern

 anonymous, bottoms            :: Symbol -> Pat
 anonymous                     =  replace anon . pattern
 bottoms                       =  replace hsUndefined . pattern



 ntArgsOf v ctx                =  args (pattern v)
   where
   args (Case e [(p, e')])     =  Case e [(p, args e')]
   args e                      =  ctx (map fst (quotesOf e))

 unique as                     =  Set.toList (Set.fromList as)


 stackConstrs                  :: [(Symbol, [Type])] -> ([([Type], String)], ST.FM Int String)
 stackConstrs xs               =  (ts, fm)
     where sxs                 =  groupBy eq2 (mergeSortBy leq2 xs)
           ts                  =  zip (map (snd . head) sxs) (map show [1 ..])
           fm                  =  ST.fromList [ (number v, show i) | (i, sx) <- zip [1 ..] sxs, (v, _) <- sx ]

 eq2 (_, b1) (_, b2)           =  b1 == b2
 leq2 (_, b1) (_, b2)          =  b1 <= b2





--                                           : [ (unCon (sts_con v), state_tcon : stack_tcon : typesOf v)
--                                             | v <- unique [ v' | (_, v', _) <- edges ], isSymbol v ])

 generate                      :: Int -> [Flag] -> Grammar
                                  -> [(Symbol, State)] -> GotoTable -> ActionTable -> [Decl]
 generate k opts grammar entries edges table



                               =  (if optCodeSize then
                                      [ DataDecl stack_tcon (
                                           (unCon empty_con, []) 
--                                           : [ ("St_" ++ s ++ "__", stack_tcon : ts)
--                                             | (ts, s) <- stTypes ]
                                           : [ ("StS_" ++ s ++ "__", state_tcon : stack_tcon : ts)
                                             | (ts, s) <- stTypes ]) ]
                                      ++ if ghcFlag then
                                             []
                                         else
                                             [ Empty
                                             , DataDecl state_tcon [ (unCon (s_con s), [])
                                                                   | (s, _) <- ST.toList table ] ]




                                  else
                                      [ DataDecl stack_tcon (
                                           (unCon empty_con, []) 
                                           : [ (unCon (con_s_s e), stack_tcon : typesOf v)
                                             | e@(s, v, s') <- edges ]) ])




                               ++ [ Empty
                                  , DataDecl nonterminal_tcon
                                       [ (name n, typesOf n) | (n, _) <- entries ] ] 
                               ++ [ Empty ]



                               ++ [ FunBind (apply (tolower (globalName (pattern n))) [tr_var | not lexFlag])
                                       (next_n s (empty_con) <>>=>
                                            Fun [apply (Con (name n)) (genVars n)]
                                                (apply hsReturn [Tuple (genVars n)]))
                                  | (n, s) <- entries ]



                               ++ concat [ Empty 
                                           : AComment (("state " ++ show (snumber s)) : reportConflicts cases)
--                                           : Sig [unVar (parse_var s)]
--                                                      (x_tcon <->> stack_tcon <->> apply (Var "Parser") [nonterminal_tcon])
--OLD                                           : Sig [unVar (parse_var s)] (apply (Con "Monad") [Var "m"] <=>>
--OLD                                                      (x_tcon <->> stack_tcon <->> apply (Var "m") [nonterminal_tcon]))






                                           : genParse_n s cases asInsert
                                         | (s, as) <- ST.toList table
                                         , let (asCopy, asInsert) = partition isCopy as
                                         , let cases = caseAnalysis k asCopy ]




                               ++ (if optCodeSize then
                                       concat [ Empty
                                                : FunBind (apply (reduce_var (rnumber r))
                                                      ([x_var] ++ [ s_var | rhs == [] ] ++ [genStack2 (revList rhs)]))
                                                      (reduceRHS' r)
                                                : if rhs == [] then
                                                      []
                                                  else
                                                      FunBind (apply (reduce_var (rnumber r))
                                                          ([x_var, st_var]))
                                                          (notpossible st_var x_var) : []
                                              | r <- productions grammar, let rhs = rrhs r ] 



                                       ++ concat [ Empty
--                                                   : Sig [unVar (goto_var v)] (apply (Con "Monad") [Var "m"] <=>>
--                                                              (state_tcon <->> x_tcon <->> stack_tcon <->> apply (Var "m") [nonterminal_tcon]))
                                                   : [ FunBind (apply (goto_var v) [s_con s])
                                                           (parse_var s')
                                                     | e@(s, v', s') <- edges, v' == v ]
                                                 | v <- nonterminals grammar ]
                                   else
                                       [])



                               ++ [ Empty
--                                  , Sig [unVar impossible_var] (apply (Con "Monad") [Var "m"] <=>>
--                                        (x_tcon <->> stack_tcon <->> apply (Var "m") [nonterminal_tcon]))
                                  , FunBind (notpossible st_var x_var) (
                                        apply hsFail [Literal "\"The `impossible' happened.\""])]
     where
     monadic                   =  Monadic  `elem` opts
     trFlag                    =  Trace    `elem` opts
     lexFlag                   =  Lexer    `elem` opts
     expFlag                   =  Expected `elem` opts
     backtrFlag                =  Backtrack `elem` opts
     optCodeSize               =  Optimize CodeSize `elem` opts

     (stTypes, stFM)           =  stackConstrs [ (v, typesOf v) | v <- terminals grammar ++ nonterminals grammar ]

     shiftOnlyFM               =  ST.fromList [ (snumber s, not (or [ nonterminal v
                                                                    | (s1, v, s2) <- edges
                                                                    , s1 == s ]))
                                              | (s, _) <- ST.toList table ]

     genParse_n s (ReduceN as) _asInsert
                               =  reduces as Nothing ++ [ impossibleCase s | not optCodeSize && backtrFlag ]
     genParse_n s (TokenCase es Nothing la) asInsert
                               =  concat [ topLevel s e (Just t) | (t, e) <- es ]
                               ++ catchall s la [ shiftRHS' e | Shift e <- asInsert ]
     genParse_n s (TokenCase es (Just b) la) asInsert
                               =  concat [ topLevel s e (Just t) | (t, e) <- es ]
                               ++ topLevel s b Nothing

     topLevel s (Shift1 e) _   =  [shift e False]
     topLevel s (ReduceN rs) t =  reduces rs t
     topLevel s (ShiftReduce e b) _
                               =  [shift e backtrFlag <||> caseexpr b]
     topLevel s b t            =  [FunBind (parse_n s st_var (genAnoPat t)) (caseexpr b)]

     caseexpr (ReduceN rs)     =  Case st_var ([ (genStack (stack r) False, reduceRHS r False) | r <- rs ] -- ####
                               ++ [(anon, notpossible st_var x_var)])
     caseexpr (ReduceReduce rs)=  foldr1 (<|>) [ Case st_var ([ (genStack (stack r) False, reduceRHS r False)]
                                                              ++ [(anon, frown (Set.empty))]) | r <- rs ] -- TODO: pass set of expected tokens
     caseexpr (TokenCase es def la)
                               =  Case tr_var ([ ( pattern x <:> tr_var, caseexpr t)
                                               | (x, t) <- es ]
                                               ++ [(anon, case def of Nothing -> frown la; Just b -> caseexpr b)])



     shift e@(s, t, _) flag    =  FunBind (parse_n s st_var (genNewPat t flag)) (shiftRHS e)

     shiftRHS e@(s, t, s')     =  trace
                                      (apply hsPutStrLn
                                          [Literal ("\"shift " ++ smangle s  ++ " (\"")
                                           <++> apply hsShow [combine (pattern t) (genVars t)]
                                           <++> Literal ("\") " ++ smangle s' ++ "\"")])
                                      (next_n s' (apply (con_s_s e) (st_var : genVars t)))

     next_n s st
         | lexFlag             =  hsGet <>>=> Fun [t'_var] (parse_n s st t'_var)
         | otherwise           =  parse_n s st tr_var



     shiftRHS' e@(s, t, s')    =  trace
                                      (apply hsPutStrLn
                                          [Literal ("\"shift " ++ smangle s  ++ " (\"")
                                           <++> apply hsShow [combine (pattern t) (genVars t)]
                                           <++> Literal ("\") " ++ smangle s' ++ "\"")])
                                      (next_n' s' (apply (con_s_s e) (st_var : genVars t)))

     next_n' s st              =  parse_n s st x_var




     genNewPat v flag
         | lexFlag             =  asPat' t_var (combine (pattern v) (genVars v))
         | isNewEOF (pattern v)=  asPat' ts_var (asPat tr_var hsNil)
         | otherwise           =  asPat' ts_var (combine (pattern v) (genVars v) <:> tr_var)
         where asPat' x p
                   | flag      =  asPat x p
                   | otherwise =  p



     reduces rs x
         | optCodeSize && equal (map pnumber rs)
                               =  [ reduce (head rs) x True ]
         | otherwise           =  [ reduce r x False | r <- rs ] -- #####

     reduce r x True           =  FunBind (parse_n (current r) st_var (genAnoPat x))
                                       (reduceRHS r True)
     reduce r x flag           =  FunBind (parse_n (current r) (genStack (stack r) flag) (genAnoPat x))
                                       (reduceRHS r flag)

     reduceRHS r@(Reduce st e@(s, v, s') _ _ i) True
                               =  apply (reduce_var i)
                                      ([x_var] ++ [ s_con (current r) | st == Nil ] ++ [st_var])
     reduceRHS r@(Reduce st e@(s, v, s') _ _ i) False
         | isStart v           =  trace
                                      (apply hsPutStrLn [Literal "\"accept\""])
                                      (apply hsReturn [ntArgsOf v (\x -> apply (Con (name v)) x)])
         | monadic             =  trace traceReduce
                                      (eval (argsOf v) (
                                          proceed_n s' (apply (con_s_s e) (st_var : genVars v))))
         | otherwise           =  trace traceReduce
                                      (proceed_n s' (ntArgsOf v (\ hole -> apply (con_s_s e) (st_var : hole))))
         where
         traceReduce           =  apply hsPutStrLn [Literal ("\"reduce by " ++ show i ++ "\"")]

         proceed_n s st'       =  parse_n s st' x_var



     reduceRHS' (Rule i v rhs _)
         | isStart v           =  trace
                                      (apply hsPutStrLn [Literal "\"accept\""])
                                      (apply hsReturn [ntArgsOf v (\x -> apply (Con (name v)) x)])
         | monadic             =  trace traceReduce
                                      (eval (argsOf v) (
                                          proceed_n (apply x (st_var : genVars v))))
         | otherwise           =  trace traceReduce
                                      (proceed_n (ntArgsOf v (\ hole -> apply x (st_var : hole))))
         where
         traceReduce           =  apply hsPutStrLn [Literal ("\"reduce by " ++ show i ++ "\"")]

         x                     =  apply (sts_con v) [s_var]

         proceed_n st'         =  apply (goto_var v) [s_var, x_var, st']




     genAnoPat Nothing         =  x_var
     genAnoPat (Just v)
         | lexFlag             =  asPat t_var (anonymous v)
         | isNewEOF (pattern v)=  asPat ts_var (asPat tr_var hsNil)
         | otherwise           =  asPat ts_var (anonymous v <:> tr_var)



     trace tr e
         | trFlag              =  tr <>>> e
         | otherwise           =  e



     catchall s la rhss        =  [ FunBind (parse_n s st_var x_var)
                                        (if null rhss then frown la else foldr1 (<|>) rhss) ]

     impossibleCase s          =  FunBind (parse_n s st_var x_var) (notpossible st_var x_var)

     frown la
         | expFlag             =  apply hsFrown [List [ bottoms t
                                                      | t <- Set.toList la ], x_var]
         | otherwise           =  apply hsFrown [x_var]

     x_var                     =  if lexFlag then t_var else ts_var

     x_tcon                    =  if lexFlag then Var "Terminal" else List [Var "Terminal"]





     genStack st False         =  genStack1 st
     genStack st True          =  genStack2 (fmap (\ (_, v, _) -> v) st)

     genStack1 Nil             =  st_var
     genStack1 (st :> e@(_, v, _))
                               =  apply (con_s_s e) (genStack1 st : argsOf v)

     genStack2 Nil             =  st_var
     genStack2 (Nil :> v)      =  apply (sts_con v) (s_var : st_var : argsOf v)
     genStack2 (st :> v)
         | onlySh              =  apply (st_con v) (genStack2 st : argsOf v)
         | otherwise           =  apply (sts_con v) (anon : genStack2 st : argsOf v)
         where onlySh          =  False -- fromJust (ST.lookup shiftOnlyFM (snumber s)) -- !!!! requires `s'




     con_s_s (s, v, s')
         | optCodeSize && onlySh
                               =  st_con v
         | optCodeSize         =  apply (sts_con v) [s_con s]
         | otherwise           =  Con ("St_" ++ smangle s ++ "_" ++ smangle s' ++ "__")
         where onlySh          =  False -- fromJust (ST.lookup shiftOnlyFM (snumber s))

     sts_con v                 =  Con ("StS_" ++ fromJust (ST.lookup stFM (number v)) ++ "__")
     st_con v                  =  Con ("St_" ++ fromJust (ST.lookup stFM (number v)) ++ "__")



     ghcFlag                   =  GHC      `elem` opts

     state_tcon | ghcFlag      =  Con "Int#"
                | otherwise    =  Con "State__"

     s_con s | ghcFlag         =  Literal (smangle s ++ "#")
             | otherwise       =  Con ("S_" ++ smangle s ++ "__")

     goto_v v s st ts          =  apply (goto_var v) [s_con s, ts, st]



     FunBind lhs rhs <||> alt  =  FunBind lhs (rhs <|> alt)

     e1 <|> e2
       | backtrFlag            =  Infix e1 "`mplus`" e2
       | otherwise             =  e1





 tolower (c : s)               =  Var (toLower c : s)

 genVars v                     =  [ v_i i | i <- [1 .. length (typesOf  v)] ]

 eval args e                   =  foldr eval e (zip [1 ..] args)
     where eval (i, ts) x      =  ts <>>=> Fun [v_i i] x

 mangle v                      =  show (number v)
 smangle s                     =  show (snumber s)
 unCon (Con x)                 =  x
 unVar (Var x)                 =  x
 asPat (Var x) p               =  As x p



 globalName (Con ('#' : s))    =  s
 globalName (App p p')         =  globalName p

 parse_n i st ts               =  apply (parse_var i) [ts, st]
 notpossible st ts             =  apply impossible_var [ts, st]

 parse_var i                   =  Var ("parse_" ++ smangle i ++ "__")
 goto_var v                    =  Var ("goto_" ++ mangle v ++ "__")
 reduce_var i                  =  Var ("reduce_" ++ show i ++ "__")
 st_var                        =  Var "st__"
 ts_var                        =  Var "ts__"
 tr_var                        =  Var "tr__"
 t_var                         =  Var "t__"
 t'_var                        =  Var "t'__"
 s_var                         =  Var "s__"
 stack_tcon                    =  Con "Stack__"
 empty_con                     =  Con "Empty__"
 nonterminal_tcon              =  Con "Nonterminal__"
 v_i i                         =  Var ("v" ++ show i ++ "__")
 impossible_var                =  Var "impossible__"
 name v                        =  globalName (pattern v) ++ "__"



 hsGet                         =  Var "get"
 hsFrown                       =  Var "frown"

 anon                          =  Var "_"
 hsUndefined                   =  Var "undefined"
 hsReturn                      =  Var "return"
 hsFail                        =  Var "fail"
 hsPutStrLn                    =  Var "putStrLn"
 hsShow                        =  Var "show"
 hsNil                         =  Con "[]"



 e1 <:> e2                     =  Infix e1 ":" e2
 e1 <++> e2                    =  Infix e1 "++" e2
 e1 <>>> e2                    =  Infix e1 ">>" e2
 e1 <>>=> e2                   =  Infix e1 ">>=" e2
 e1 <=>> e2                    =  Infix e1 "=>" e2
 infixr <->>
 e1 <->> e2                    =  Infix e1 "->" e2
