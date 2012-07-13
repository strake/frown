> module MLexer1 ( module Terminal1, module MLexer1 ) where
> import Terminal1
> import Char
>
> type CPS a answer             =   (a -> answer) -> answer
>
> newtype Lex m a               =   Lex { unLex :: forall ans . CPS a (String -> m ans) }
>
> instance (Monad m) => Monad (Lex m) where
>     return a                  =   Lex (\ cont -> cont a)
>     m >>= k                   =   Lex (\ cont -> unLex m (\ a -> unLex (k a) cont))
>     fail s                    =   lift (fail s)
>
> lift                          ::  (Monad m) => m a -> Lex m a
> lift m                        =   Lex (\ cont inp -> m >>= \ a -> cont a inp)
>
> run                           ::  (Monad m) => Lex m a -> (String -> m a)
> run parser inp                =   unLex parser (\ a rest -> return a) inp
>
> get                           ::  (Monad m) => Lex m Terminal
> get                           =
>   Lex (\ cont inp ->
>     let lexer []              =   cont (EOF)          []
>         lexer ('+'  : cs)     =   cont (Addop Plus)   cs
>         lexer ('-'  : cs)     =   cont (Addop Minus)  cs
>         lexer ('*'  : cs)     =   cont (Mulop Times)  cs
>         lexer ('/'  : cs)     =   cont (Mulop Divide) cs
>         lexer ('='  : cs)     =   cont (Equal)        cs
>         lexer ('('  : cs)     =   cont (LParen)       cs
>         lexer (')'  : cs)     =   cont (RParen)       cs
>         lexer (c : cs)
>           | isSpace c         =   lexer cs
>           | isAlpha c         =   let (s, cs') = span isAlphaNum  cs in cont (ident    (c : s)) cs'
>           | isDigit c         =   let (s, cs') = span isDigit     cs in cont (numeral  (c : s)) cs'
>           | otherwise         =   lexer cs
>     in  lexer inp)
>
> frown                         ::  (Monad m) => Terminal -> Lex m a
> frown t                       =   Lex (\ cont inp ->
>                                     fail ("\n*** syntax error:\n" ++ context 4 inp))
>
> context                       ::  Int -> String -> String
> context n inp                 =   unlines (take n (lines inp ++ ["<end of input>"]))