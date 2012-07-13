        ghc --make -o sessions Sessions.lhs

> import System
> import Char

> main                          =  do args <- getArgs
>                                     mapM session args

> session fname                 =  do cnts <- readFile fname
>                                     cnts' <- readFile (base ++ "in")
>                                     --putStrLn  (crop cnts' cnts)
>                                     writeFile (base ++ "session") (crop cnts' cnts)
>   where
>   (base, ext)                 =  revBreak (== '.') fname
>   base'                       =  init base

>   crop                        :: String -> String -> String
>   crop cnts'                  =  unlines
>                               .  concat
>                               .  map format
>                               .  zipWith (:) (lines cnts')
>                               .  tail
>                               .  group
>                               .  filter (not . all isSpace)
>                               .  init . tail
>                               .  dropWhile (/= "Type :? for help")
>                               .  lines
>
>   group []                    =  [[]]
>   group (s : x)
>     | base' `isPrefix` s      =  [] : tack (drop (length base' + 2) s) (group x) -- remove @File> @
>     | otherwise               =  tack s (group x)
>
>   format []                   =  []
>   format (s : x)              =  ("> " ++ base' ++ ">> " ++ s) : format' x
>
>   format' []                  =  []
>   format' (s : x)
>     | warning `isPrefix` s    =  quote s : format' x
>     | prgerror `isPrefix` s   =  quote s : map quote x
>     | otherwise               =  ("> " ++ s) : format' x

> revBreak                      :: (a -> Bool) -> [a] -> ([a], [a])
> revBreak p as                 =  (reverse as2, reverse as1)
>     where (as1, as2)          =  break p (reverse as)

> isPrefix                      :: (Eq a) => [a] -> [a] -> Bool
> p `isPrefix` as               =  p == take (length p) as

> prgerror                      =  "Program error: "

> warning                       =  "Warning: "

> tack a (x : xs)               =  (a : x) : xs

> quote s                       =  "> '" ++ s ++ "'"