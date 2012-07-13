Compile me with

        ghc --make TestFrown.lhs

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
> frown                         =  "../../frown"
>
> test opts g flag              =  do echo (out ++ g ++ " ..." ++
>                                           if null opts then ""
>                                           else " (" ++ interleave ", " [ o | o <- opts ] ++ ")")
>                                     call ([frown] ++ [ " " ++ o | o <- opts' ] ++ [" ", g])
>                                     call ["hugs +I -98 ", t, ".hs < ", t, ".in ", diff, t, ".out"]
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
>     [ ("Calc.lg",     False, True,  [])
>     , ("Let1.lg",     False, True,  ["backtrack"])
>     , ("Let2.lg",     False, True,  [])
>     , ("Let3.lg",     False, True,  [])
>     , ("Let4.lg",     True,  True,  ["lexer"])
>     , ("Let5.lg",     True,  True,  ["lexer"])
>     , ("Let6.lg",     True,  True,  ["expected", "lexer", "optimize"]) -- -o to ensure that the error messages are identical
>     , ("Let7.lg",     True,  True,  ["lexer"])
>     , ("Let8.lg",     True,  True,  ["lexer"])
>     , ("MCalc.lg",    False, True,  [])
>     , ("Paren1.lg",   False, True,  [])
>     , ("Paren2.lg",   False, True,  [])
>     , ("Paren3.lg",   False, True,  [])
>     , ("RepMin.lg",   False, True,  [])
>     , ("VarCalc.lg",  False, True,  [])
>     , ("VarParen.lg", False, True,  [])
>     ]

> main                          =
>     do args <- getArgs
>        case args of
>            ["--generate"]         -> generate grammars >> return ()
>            ("--generate" : files) -> generate [ g | g@(s, _, _, _) <- grammars, s `elem` files] >> return ()
>            []                     -> testall  >> return ()

> generate grammars             =
>     foreach grammars (\ (g, eof, lalr, flags) ->
>       test flags g False
>     )
>
> testall                       =
>     foreach grammars (\ (g, eof, lalr, flags) ->
>       foreach ["standard", "compact", "stackless"] (\ fmt -> -- , "gvstack"
>         foreach (subsets [ "optimize" -- ,"signature=mono", "signature=poly"
>                          , "prefix", "prefix=frown", "suffix=_"]) (\ opts ->
>            let opts' = flags ++ opts ++ ["code=" ++ fmt] in
>            unless ((fmt == "gvstack" && not eof)
>                    || (fmt `elem` ["stackless", "gvstack"] && "backtrack" `elem` flags)
>                    || (fmt == "gvstack" && "lookahead=2" `elem` flags)
>                    || (fmt `elem` ["stackless", "gvstack"] && not lalr))
>                (test opts' g True)
>         )
>       )
>     )