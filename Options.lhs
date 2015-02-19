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

> module Options                (  Format(..), Flag(..)
>                               , lookahead, prefix, suffix, verbose, options  )
> where
> import System.Console.GetOpt
> import Data.Char
> import Data.List ( isPrefixOf )
> import System.IO

%-------------------------------=  --------------------------------------------
\section{Command line options}
%-------------------------------=  --------------------------------------------

> data Format                   =  Standard | Compact | Stackless | GVStack
>                                  deriving (Eq)
>
> data Flag                     =  Backtrack
>                               |  Code Format
>                               |  Copying
>                               |  Debug
>                               |  Expected
>                               |  GHC
>                               |  Help
>                               |  Info
>                               |  Lookahead Int
>                               |  Lexer String
>                               |  Noinline
>                               |  Optimize
>                               |  Pagewidth Int
>                               |  Prefix String
>                               |  Signature Bool -- `|True|' means polymorphic
>                               |  Suffix String
>                               |  Trace
>                               |  Verbose
>                               |  Version
>                               |  Warranty
>                                  deriving (Eq)

> lookahead                     :: [Flag] -> Int
> lookahead opts                =  head ([ k | Lookahead k <- opts ] ++ [1]) `max` 1

> prefix                        :: [Flag] -> String
> prefix opts                   =  head ([ s | Prefix s <- opts ] ++ [""])
>
> suffix                        :: [Flag] -> String
> suffix opts                   =  head ([ s | Suffix s <- opts ] ++ ["__"])

> verbose                       :: [Flag] -> String -> IO ()
> verbose opts s
>     | Verbose `elem` opts     =  hPutStrLn stdout s
>     | otherwise               =  return ()

> options                       :: [OptDescr Flag]
> options                       =  [ Option "b" ["backtrack"] (NoArg Backtrack)
>                                        "generate a backtracking parser"
>                                  , Option "c" ["code"] (OptArg getCode "compact|gvstack|stackless|standard")
>                                        "select output format"
>                                  , Option "" ["copying"] (NoArg Copying)
>                                        "display details of copying"
>                                  , Option "d" ["debug"] (NoArg Debug)
>                                        "emit debugging information"
>                                  , Option "e" ["expected"] (NoArg Expected)
>                                        "pass a list of expected terminals to `frown'"
>                                  , Option "g" ["ghc"] (NoArg GHC)
>                                        "use GHC extensions"
>                                  , Option "h?" ["help"] (NoArg Help)
>                                        "print this information"
>                                  , Option "i" ["info"] (NoArg Info)
>                                        "put additional information into generated file"
>                                  , Option "k" ["lookahead"] (OptArg getLookahead "nat")
>                                        "use k tokens of lookahead"
>                                  , Option "l" ["lexer"] (ReqArg Lexer "identifier")
>                                        "use a monadic lexer (`get :: M Terminal')"
>                                  , Option "n" ["noinline"] (NoArg Noinline)
>                                        "generate NOINLINE pragmas"
>                                  , Option "O" ["optimize"] (NoArg Optimize)
>                                        "optimize parser"
>                                  , Option "p" ["pagewidth"] (OptArg getPagewidth "nat")
>                                        "use the specified pagewidth for pretty printing"
>                                  , Option "" ["prefix"] (OptArg getPrefix "string")
>                                        "use prefix for frown generated variables"
>                                  , Option "s" ["signature"] (OptArg getSignature "mono|poly")
>                                        "add type signatures"
>                                  , Option "" ["suffix"] (OptArg getSuffix "string")
>                                        "use suffix for frown generated variables"
>                                  , Option "t" ["trace"] (NoArg Trace)
>                                        "insert calls to tracing routines (`shift', `reduce' and `accept')"
>                                  , Option "v" ["verbose"] (NoArg Verbose)
>                                        "be verbose"
>                                  , Option "" ["version"] (NoArg Version)
>                                        "print version information"
>                                  , Option "" ["warranty"] (NoArg Warranty)
>                                        "display details of warranty" ]

> getCode, getLookahead, getPagewidth, getPrefix, getSuffix, getSignature
>                               :: Maybe String -> Flag
> getCode Nothing               =  Code Standard
> getCode (Just s)
>   | s << "compact"            =  Code Compact
>   | s << "gvstack"            =  Code GVStack
>   | s << "stackless"          =  Code Stackless
>   | otherwise                 =  Code Standard
>
> getLookahead Nothing          =  Lookahead 2
> getLookahead (Just s)         =  Lookahead (read s)
>
> getPagewidth Nothing          =  Pagewidth 80
> getPagewidth (Just s)         =  Pagewidth (read s)
>
> getPrefix Nothing             =  Prefix ""
> getPrefix (Just s)            =  Prefix s
>
> getSuffix Nothing             =  Suffix ""
> getSuffix (Just s)            =  Suffix s

> getSignature Nothing          =  Signature False
> getSignature (Just s)
>   | s << "mono"               =  Signature False
>   | s << "poly"               =  Signature True
>   | otherwise                 =  Signature False

> (<<)                          :: String -> String -> Bool
> s << t                        =  map toLower s `isPrefixOf` map toLower t