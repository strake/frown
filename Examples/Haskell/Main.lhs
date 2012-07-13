Compile me with

--        ghc --make -O -package lang -package util -package text Main.lhs +RTS -c -M120M
        ghc --make -O -package util -package text Main.lhs +RTS -c -M120M

        hmake -nhc -ILib -LLib Main

ToDo: Are initial values for SrcLoc/current column correct?

> module Main (testLexer, main) where

> import IO
> import HsLexer
> import HsParseMonad
> import HsParser
> import HsSyn
> import HsPretty
> -- import PrettyM
> import System
> import GetOpt

> data Flag = LexOnlyLength          -- print number of tokens only
>           | LexOnlyRev             -- print tokens in reverse order
>           | LexOnly                -- print tokens
>           | ParseLength            -- print number of declarations only
>           | ParseInternal          -- print abstract syntax in internal format
>           | ParsePretty PPLayout   -- pretty print in this style
>           | Help                   -- give short usage info

> usage = "usage: hsparser [option] [filename]\n"

> options =
>    [ Option ['n']  ["numtokens"] (NoArg LexOnlyLength) "print number of tokens only",
>      Option ['r']  ["revtokens"] (NoArg LexOnlyRev)    "print tokens in reverse order",
>      Option ['t']  ["tokens"]    (NoArg LexOnly)       "print tokens",
>      Option ['d']  ["numdecls"]  (NoArg ParseLength)   "print number of declarations only",
>      Option ['a']  ["abstract"]  (NoArg ParseInternal) "print abstract syntax in internal format",
>      Option ['p']  ["pretty"]    (OptArg style "STYLE")   "pretty print in STYLE[(o)ffside|(s)emicolon|(i)nline|(n)one](default = offside)",
>      Option ['h','?'] ["help"]   (NoArg Help)          "display this help and exit"]

> style :: Maybe String -> Flag
> style Nothing = ParsePretty PPOffsideRule
> style (Just s) = ParsePretty $ case s of
>				    "o" -> PPOffsideRule
>				    "offside" -> PPOffsideRule
>				    "s" -> PPSemiColon
>				    "semicolon" -> PPSemiColon
>				    "i" -> PPInLine
>				    "inline" -> PPInLine
>				    "n" -> PPNoLayout
>				    "none" -> PPNoLayout
>				    _ -> PPOffsideRule

> main :: IO ()
> main = do cmdline <- getArgs
>           mainHugs cmdline

> mainHugs :: [String] -> IO ()
> mainHugs cmdline =
>    case getOpt Permute options cmdline of
>       (flags, [], [])     -> do inp <- getContents
>                                 putStrLn (handleFlag (getFlag flags) inp)
>       (flags, args, [])     -> sequence_ [ readFile f >>= \ inp -> putStrLn (handleFlag (getFlag flags) inp) | f <- args ]
>       (_,     _,    errors) -> error (concat errors ++ usageInfo usage options)

> getFlag :: [Flag] -> Flag
> getFlag []  = ParsePretty PPOffsideRule
> getFlag [f] = f
> getFlag _   = error usage

> handleFlag :: Flag -> String -> String
> handleFlag LexOnlyLength = show . numToks . testLexerRev
> handleFlag LexOnlyRev    = show . testLexerRev
> handleFlag LexOnly       = show . testLexer
> handleFlag ParseLength   = show . allLengths . testParser
>    where allLengths (HsModule _ _ imp d) = length imp + length d
> handleFlag ParseInternal = show . testParser
> handleFlag (ParsePretty l) = renderWithMode defaultMode{layout = l} 
>				    . ppHsModule . testParser
> handleFlag Help          = const $ usageInfo ("A simple test program for *The Haskell Parser*\n" ++ usage) options

> numToks :: Result [Token] -> Int
> numToks (Fail err) = error ("Huh? " ++ err)
> numToks (Return toks)  = length toks

> testLexerRev :: String -> Result [Token]
> testLexerRev s = runTokens (loop []) s (SrcLoc 1 0) 1 []   -- this magic should be abstracted out...
>   where loop toks =
>	     get >>= \t -> case t of 
>			       EOF -> return toks
>			       _   -> loop (t:toks)

> testLexer :: String -> Result [Token]
> testLexer s = runTokens (loop []) s (SrcLoc 1 0) 1 []
>   where loop toks =
>	     get >>= \t -> case t of 
>			       EOF -> return (reverse toks) -- space leak?
>			       _   -> loop (t:toks)

> testParser :: String -> HsModule
> testParser s = case runHsModule xmodule s (SrcLoc 1 1) 0 [] of
>		    Return e -> e
>		    Fail err -> error err