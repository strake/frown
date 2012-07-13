%if False

> module MLexer3 ( module Terminal1, module MLexer3 ) where
> import Terminal1
> import Char
> import List
>
> type CPS a answer             =  (a -> answer) -> answer
>
> newtype Lex m a               =  Lex { unLex :: forall ans . CPS a (String -> Int -> String -> m ans) }
>
> instance (Monad m) => Monad (Lex m) where
>     return a                  =  Lex (\ cont -> cont a)
>     m >>= k                   =  Lex (\ cont -> unLex m (\ a -> unLex (k a) cont))
>     fail s                    =  lift (fail s)
>
> lift                          :: (Monad m) => m a -> Lex m a
> lift m                        =  Lex (\ cont inp line cur -> m >>= \ a -> cont a inp line cur)
>
> run                           :: (Monad m) => Lex m a -> (String -> m a)
> run parser inp                =  unLex parser (\ a rest line cur -> return a) inp 1 (current inp)
>
> current                       :: String -> String
> current s                     =  takeWhile (/= '\n') s
>
> get                           :: (Monad m) => Lex m Terminal
> get                           =
>   Lex (\ cont inp line cur ->
>     let lexer []          n x =  cont (EOF)          [] n       x
>         lexer ('\n' : cs) n x =  lexer               cs (n + 1) (current cs)
>         lexer ('+'  : cs) n x =  cont (Addop Plus)   cs n       x
>         lexer ('-'  : cs) n x =  cont (Addop Minus)  cs n       x
>         lexer ('*'  : cs) n x =  cont (Mulop Times)  cs n       x
>         lexer ('/'  : cs) n x =  cont (Mulop Divide) cs n       x
>         lexer ('='  : cs) n x =  cont (Equal)        cs n       x
>         lexer ('('  : cs) n x =  cont (LParen)       cs n       x
>         lexer (')'  : cs) n x =  cont (RParen)       cs n       x
>         lexer (c : cs)    n x
>           | isSpace c         =  lexer cs n x
>           | isAlpha c         =  let (s, cs') = span isAlphaNum cs in  cont (ident   (c : s)) cs' n x
>           | isDigit c         =  let (s, cs') = span isDigit    cs in  cont (numeral (c : s)) cs' n x
>           | otherwise         =  fail ("\n*** lexical error at "
>                                        ++ position cs n x ++ ":\n"
>                                        ++ context 4 cs x)
>     in  lexer inp line cur)

%endif

> frown                         :: (Monad m) => [String] -> Terminal -> Lex m a
> frown la t                    =  Lex (\ cont inp line cur ->
>                                    fail ("\n*** syntax error at "
>                                          ++ position inp line cur ++ ":\n"
>                                          ++ context 4 inp cur
>                                          ++ "* expected: " ++ concat (intersperse ", " la)))

%if False

> position                      :: String -> Int -> String -> String
> position inp line cur         =  "(line "  ++ show line ++ ", column " ++ show col ++ ")"
>     where col                 =  length cur - length (current inp)
>
> context                       :: Int -> String -> String -> String
> context n inp cur             =  unlines ([cur, replicate col' ' ' ++ "^"]
>                                           ++ take n (lines (drop 1 (dropWhile (/= '\n') inp))
>                                                      ++ ["<end of input>"]))
>     where col'                =  length cur - length (current inp) - 1

%endif