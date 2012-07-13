






























 module Options                (  Format(..), Flag(..)
                               , lookahead, prefix, suffix, verbose, options  )
 where
 import Base
 import GetOpt
 import Char
 import IO





 data Format                   =  Standard | Compact | Stackless | GVStack
                                  deriving (Eq)

 data Flag                     =  Backtrack
                               |  Code Format
                               |  Copying
                               |  Debug
                               |  Expected
                               |  GHC
                               |  Help
                               |  Info
                               |  Lookahead Int
                               |  Lexer
                               |  Noinline
                               |  Optimize
                               |  Pagewidth Int
                               |  Prefix String
                               |  Signature Bool -- `|True|' means polymorphic
                               |  Suffix String
                               |  Trace
                               |  Verbose
                               |  Version
                               |  Warranty
                                  deriving (Eq)

 lookahead                     :: [Flag] -> Int
 lookahead opts                =  head ([ k | Lookahead k <- opts ] ++ [1]) `max` 1

 prefix                        :: [Flag] -> String
 prefix opts                   =  head ([ s | Prefix s <- opts ] ++ [""])

 suffix                        :: [Flag] -> String
 suffix opts                   =  head ([ s | Suffix s <- opts ] ++ ["__"])

 verbose                       :: [Flag] -> String -> IO ()
 verbose opts s
     | Verbose `elem` opts     =  hPutStrLn stdout s
     | otherwise               =  return ()

 options                       :: [OptDescr Flag]
 options                       =  [ Option "b" ["backtrack"] (NoArg Backtrack)
                                        "generate a backtracking parser"
                                  , Option "c" ["code"] (OptArg getCode "compact|gvstack|stackless|standard")
                                        "select output format"
                                  , Option "" ["copying"] (NoArg Copying)
                                        "display details of copying"
                                  , Option "d" ["debug"] (NoArg Debug)
                                        "emit debugging information"
                                  , Option "e" ["expected"] (NoArg Expected)
                                        "pass a list of expected terminals to `frown'"
                                  , Option "g" ["ghc"] (NoArg GHC)
                                        "use GHC extensions"
                                  , Option "h?" ["help"] (NoArg Help)
                                        "print this information"
                                  , Option "i" ["info"] (NoArg Info)
                                        "put additional information into generated file"
                                  , Option "k" ["lookahead"] (OptArg getLookahead "nat")
                                        "use k tokens of lookahead"
                                  , Option "l" ["lexer"] (NoArg Lexer)
                                        "use a monadic lexer (`get :: M Terminal')"
                                  , Option "n" ["noinline"] (NoArg Noinline)
                                        "generate NOINLINE pragmas"
                                  , Option "o" ["optimize"] (NoArg Optimize)
                                        "optimize parser"
                                  , Option "p" ["pagewidth"] (OptArg getPagewidth "nat")
                                        "use the specified pagewidth for pretty printing"
                                  , Option "" ["prefix"] (OptArg getPrefix "string")
                                        "use prefix for frown generated variables"
                                  , Option "s" ["signature"] (OptArg getSignature "mono|poly")
                                        "add type signatures"
                                  , Option "" ["suffix"] (OptArg getSuffix "string")
                                        "use suffix for frown generated variables"
                                  , Option "t" ["trace"] (NoArg Trace)
                                        "insert calls to tracing routines (`shift', `reduce' and `accept')"
                                  , Option "v" ["verbose"] (NoArg Verbose)
                                        "be verbose"
                                  , Option "" ["version"] (NoArg Version)
                                        "print version information"
                                  , Option "" ["warranty"] (NoArg Warranty)
                                        "display details of warranty" ]

 getCode, getLookahead, getPagewidth, getPrefix, getSuffix, getSignature
                               :: Maybe String -> Flag
 getCode Nothing               =  Code Standard
 getCode (Just s)
   | s << "compact"            =  Code Compact
   | s << "gvstack"            =  Code GVStack
   | s << "stackless"          =  Code Stackless
   | otherwise                 =  Code Standard

 getLookahead Nothing          =  Lookahead 2
 getLookahead (Just s)         =  Lookahead (read s)

 getPagewidth Nothing          =  Pagewidth 80
 getPagewidth (Just s)         =  Pagewidth (read s)

 getPrefix Nothing             =  Prefix ""
 getPrefix (Just s)            =  Prefix s

 getSuffix Nothing             =  Suffix ""
 getSuffix (Just s)            =  Suffix s

 getSignature Nothing          =  Signature False
 getSignature (Just s)
   | s << "mono"               =  Signature False
   | s << "poly"               =  Signature True
   | otherwise                 =  Signature False

 (<<)                          :: String -> String -> Bool
 s << t                        =  map toLower s `isPrefix` map toLower t
