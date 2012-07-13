Compile me with

        ghc --make TestFrown.lhs

Missing grammar files:

        local.g

> import System
> import Monad

Helper functions.

> subsets []                    =  [[]]
> subsets (a : s)               =  sets ++ map (a :) sets
>     where sets                =  subsets s
>
> interleave sep []             =  ""
> interleave sep [s]            =  s
> interleave sep (s1 : s2 : x)  =  s1 ++ sep ++ interleave sep (s2 : x)

> revBreak                      :: (a -> Bool) -> [a] -> ([a], [a])
> revBreak p as                 =  (reverse as2, reverse as1)
>     where (as1, as2)          =  break p (reverse as)
>
> echo                          =  putStrLn
>
> foreach x f                   =  mapM f x
>
> call xs                       =  system cmd >>= \ exit ->
>                                      case exit of
>                                          ExitSuccess   -> return ()
>                                          ExitFailure _ -> putStrLn ("!!! failed: " ++ cmd)
>     where cmd                 =  concat xs
>
> frown                         =  "../frown"
>
> test opts g flag              =  do echo (out ++ g ++ " ..." ++
>                                           if null opts then ""
>                                           else " (" ++ interleave ", " [ o | o <- opts ] ++ ")")
>                                     call ([frown] ++ [ " " ++ o | o <- opts' ] ++ [" ", g])
>                                     call ["hugs -98 ", t, ".hs < ", t, ".in ", diff, t, ".out"]
>     where out | flag          =  "* testing "
>               | otherwise     =  "* generating "
>           opts'               =  map ("--" ++) opts
>           (s, _)              =  revBreak (== '.') g
>           t                   =  init s
>           diff | flag         =  "| diff -q - "
>                | otherwise    =  "> "

NB. The `|-98|' is only needed for `|LexTerm|'.

Grammar files (has `|EOF|' symbol; is LALR, necessary flags).

> grammars                      =
>     [ ("Ambiguous.g",  False, True,  ["backtrack"])
>     , ("Calc.g",       True,  True,  [])
>     , ("Conflict.g",   False, True,  [])
>     , ("Conflict2.g",  False, True,  [])
>     , ("Conflict3.g",  False, True,  [])
>     , ("Dangling.g",   True,  True,  [])
>     , ("Dead.g",       False, True,  [])
>     , ("Empty.g",      False, True,  [])
>     , ("Epsilon.g",    False, True,  [])
>     , ("Expr.lg",      True,  True,  [])
>     , ("Fun.g",        False, True,  ["backtrack"])
>     , ("IExpr.lg",     False, True,  ["expected"])
>     , ("IfThenElse.g", True,  True,  [])
>     , ("Imports.lg",   False, True,  [])
>     , ("IParen.lg",    False, True,  [])
>     , ("LA.g",         False, False, [])
>     , ("LexTerm.g",    True,  True,  ["lexer", "expected", "signature"])
>     , ("Light.g",      True,  True,  [])
>     , ("List.lg",      False, True,  [])
>     , ("Loop.g",       True,  True,  [])
> --    , ("Loop1.g",      True,  True,  []) -- always loops!
>     , ("Loop2.g",      True,  True,  [])
>     , ("LR.g",         False, False, [])
>     , ("LR1.g",        False, False, [])
>     , ("LValue.g",     False, True,  [])
>     , ("Markup.lg",    False, True,  [])
>     , ("MCalc.g",      True,  True,  [])
>     , ("Overlapping.g",False, True,  [])
>     , ("Paren.lg",     False, True,  [])
>     , ("PrecExpr.g",   False, True,  [])
>     , ("Prec2Expr.g",  False, True,  [])
>     , ("RepMin.lg",    False, True,  [])
>     , ("Ross.lg",      False, True,  [])
>     , ("Schemes.lg",   False, True,  [])
>     , ("Simple.g",     False, True,  [])
>     , ("Stat.g",       False, True,  [])
>     , ("Term.g",       True,  True,  [])
>     , ("TermX.g",      False, True,  [])
>     , ("Test.g",       True,  True,  [])
>     , ("Trace.g",      False, True,  [])
>     , ("Tuple.lg",     False, True,  [])
>     , ("Type.g",       False, True,  ["lookahead=2"])
>     , ("VarExpr.g",    False, True,  [])
>     , ("Var2Expr.g",   False, True,  [])
>     , ("VarParen.lg",  False, True,  [])
>     , ("VarType.g",    False, True,  ["lookahead=2"])
>     ]

> main                          =
>     do args <- getArgs
>        case args of
>            ["--generate"]    -> generate >> return ()
>            ["--generate", f] -> sequence_ [ test flags g False | (g, eof, lalr, flags) <- grammars, f == g ]
>            []                -> testall  >> return ()
>            [f]               -> sequence_ [ test' g eof lalr flags | (g, eof, lalr, flags) <- grammars, f == g ]

> generate                      =
>     foreach grammars (\ (g, eof, lalr, flags) ->
>       test flags g False
>     )
>
> testall                       =
>     foreach grammars (\ (g, eof, lalr, flags) ->
>       test' g eof lalr flags
>     )

> test' g eof lalr flags        =
>       foreach ["standard", "compact", "stackless", "gvstack"] (\ fmt ->
>--       foreach ["standard"] (\ fmt ->
>         foreach (subsets [ "optimize", "signature=mono", "signature=poly"
>                          , "prefix", "prefix=frown", "suffix=_"]) (\ opts ->
>--         foreach (subsets [ "optimize" ]) (\ opts ->
>--         foreach (subsets []) (\ opts ->
>            let opts' = flags ++ opts ++ ["code=" ++ fmt] in
>            unless ((fmt == "gvstack" && not eof)
>                    || (fmt `elem` ["stackless", "gvstack"] && "backtrack" `elem` flags)
>                    || (fmt == "gvstack" && "lookahead=2" `elem` flags)
>                    || (fmt `elem` ["stackless", "gvstack"] && not lalr))
>                (test opts' g True)
>         )
>       )


