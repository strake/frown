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

%-------------------------------=  --------------------------------------------
\subsection{A Haskell lexer}
%-------------------------------=  --------------------------------------------

> module Lexer		        (  Token(..), isVarid, isConid, isNotSpace
>                               ,  isWhite, toString, tokenize, next  )
> where
> import Prettier               hiding (  string, concat  )
> import qualified Prettier as PP
> import Prelude                hiding (  lex  )
> import Data.Char              hiding (  lexLitChar, Space, isSymbol  )
> import qualified Data.Char    (isSymbol)
> import System.IO
> import Control.Applicative ((<|>))
> import Control.Monad
> import qualified Control.Monad.Fail
> import Base
> import Options

> instance Control.Monad.Fail.MonadFail Result where
>   fail _ = mzero

A simple Haskell lexer, essentially a modification of the Prelude
function |lex|.

> tokenize			:: [Flag] -> [Char] -> IO [Token]
> tokenize opts str             =  do verb "* Lexing ..."
>                                     verb ("  " ++ show (length ts) ++ " tokens")
>                                     return ts
>     where
>     ts			=  tidyup (qualify (lexify str))
>     verb                      =  verbose opts

The lexer functions are pure; lexical errors are propagated as |Error|
tokens.

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Tokens}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> data Token			=  Space String
>				|  Conid String
>				|  Varid String
>				|  Consym String
>				|  Varsym String
>				|  Numeral String
>				|  Char String
>				|  String String
>				|  Special Char
>				|  Comment String
>				|  Nested String
>				|  Keyword String
>				|  Qual String Token
>				|  Op Token
>                               |  LQuote
>                               |  RQuote
>                               |  Quote [Token]
>                               |  Unquote [Token]
>                               |  Error String
>                               |  EOF
>				   deriving (Eq, Ord, Show)
>
> instance Pretty Token where
>     prettyPrec d (Quote ts)   =  condParens (d > 9) (
>                                      PP.string "Quote"   <+> nest 6 (prettyPrec 10 ts))
>     prettyPrec d (Unquote ts) =  condParens (d > 9) (
>                                      PP.string "Unquote" <+> nest 8 (prettyPrec 10 ts))
>     prettyPrec d t            =  PP.string (showsPrec d t "")

> isVarid, isConid, isNotSpace	:: Token -> Bool
> isVarid (Varid _)		=  True
> isVarid (Qual _ t)		=  isVarid t
> isVarid _			=  False
>
> isConid (Conid _)		=  True
> isConid (Qual _ t)		=  isConid t
> isConid _			=  False
>
> isNotSpace (Space _)		=  False
> isNotSpace _			=  True
>
> isWhite                       :: Token -> Bool
> isWhite (Space _)		=  True
> isWhite (Comment _)	        =  True
> isWhite (Nested _)	        =  True
> isWhite _			=  False

> toString			:: Token -> String
> toString (Space s)		=  s
> toString (Conid s)		=  s
> toString (Varid s)		=  s
> toString (Consym s)		=  s
> toString (Varsym s)		=  s
> toString (Numeral s)		=  s
> toString (Char s)		=  s
> toString (String s)		=  s
> toString (Special c)		=  [c]
> toString (Comment s)		=  "--" ++ s
> toString (Nested s)		=  "{-" ++ s ++ "-}"
> toString (Keyword s)		=  s
> toString (Qual m s)		=  m ++ "." ++ toString s
> toString (Op s)		=  "`" ++ toString s ++ "`"
> toString LQuote		=  "%{"
> toString RQuote		=  "}%"
> toString (Quote ts)           =  "%{" ++ concatMap toString ts ++ "}%"
> toString (Unquote ts)         =  "{" ++ concatMap toString ts ++ "}"
> toString (Error _)            =  "" -- errors are not shown
> toString EOF                  =  "<end of input>"

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Phase 1}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> lexify			:: [Char] -> [Token]
> lexify []			=  []
> lexify s@(_ : _)		=  case lex s of
>     Fail m			-> [Error ("lexical error: " ++ m ++ "\n<...> " ++ next 3 s)]
>     Return (t, s')		-> t : lexify s'

> next                          :: Int -> String -> String
> next n                        =  unlines . take n . lines

> lex				:: String -> Result (Token, String)
> lex ""			=  fail "unexpected end of input"
> lex ('\'' : s)		=  do let (t, u) = lexLitChar s
>				      v <- match "\'" u
>				      return (Char ("'" ++ t ++ "'"), v)
>                                  `mplus` fail "character literal"
> lex ('"' : s)		        =  do let (t, u) = lexLitStr s
>				      v <- match "\"" u
>				      return (String ("\"" ++ t ++ "\""), v)
>                                  `mplus` fail "string literal"
> lex ('-' : '-' : s)		=  let (t, u) = break (== '\n') s
>				   in  return (Comment t, u)
> lex ('{' : '-' : s)		=  do let (t, u) = nested 0 s
>				      v <- match "-}" u
>				      return (Nested t, v)
>                                  `mplus` fail "missing `-}'"
> lex ('%' : '{' : s)           =  return (LQuote, s)
> lex ('}' : '%' : s)           =  return (RQuote, s)
> lex (c : s)
>     | isSpace c		=  let (t, u) = span isSpace s in return (Space (c : t), u)
>     | isSpecial c		=  return (Special c, s)
>     | isUpper c		=  let (t, u) = span isIdChar s in return (Conid (c : t), u)
>     | isLower c || c == '_'	=  let (t, u) = span isIdChar s in return (Varid (c : t), u)
>     | c == ':'		=  let (t, u) = span isSymbol s in return (Consym (c : t), u)
>     | isSymbol c		=  let (t, u) = span isSymbol s in return (Varsym (c : t), u)
>     | isDigit c		=  do let (ds, t) = span isDigit s
>			              (fe, u)  <- lexFracExp t
>				      return (Numeral (c : ds ++ fe), u)
>                                  `mplus` fail "numeral"
>     | otherwise		=  fail "strange character"
> {-
>     where
>     classify x
>         | x `elem` keywords	=  Keyword x
>         | otherwise		=  Varid   x
> -}
>
>
> lexFracExp			:: String -> Result (String, String)
> lexFracExp s			=  do t <- match "." s
>				      (ds, u) <- lexDigits' t
>				      (e, v)  <- lexExp u
>				      return ('.' : ds ++ e, v)
>				`mplus` return ("", s)
>
> lexExp			:: String -> Result (String, String)
> lexExp (e:s)
>      | e `elem` "eE" 		=
>            [(e : c : ds, u) | c : t <- pure s
>                             , c `elem` "+-"
>                             , (ds, u) <- lexDigits' t] <|>
>            [(e : ds, t) | (ds, t) <- lexDigits' s]
> lexExp s			=  return ("", s)
>
> lexDigits'			:: String -> Result (String, String)
> lexDigits' s			=  do (cs@(_ : _), t) <- return (span isDigit s)
>                                     return (cs, t)

> match				:: String -> String -> Result String
> match p s
>     | p == t			=  return u
>     | otherwise		=  mzero
>     where (t, u)		=  splitAt (length p) s

> nested			:: Int -> String -> (String, String)
> nested _     []		=  ([], [])
> nested 0     ('-' : '}' : s)	=  ([], '-':'}':s)
> nested n     ('-' : '}' : s)	=  '-' <| '}' <| nested (n - 1) s
> nested n     ('{' : '-' : s)	=  '{' <| '-' <| nested (n + 1) s
> nested n     (c : s)		=  c <| nested n s

> lexLitChar, lexLitStr		:: String -> (String, String)
> lexLitChar []			=  ([], [])
> lexLitChar ('\'' : s)		=  ([], '\'' : s)
> lexLitChar ('\\' : c : s)	=  '\\' <| c <| lexLitChar s
> lexLitChar (c : s)		=  c <| lexLitChar s
>
> lexLitStr []			=  ([], [])
> lexLitStr ('"' : s)		=  ([], '"' : s)
> lexLitStr ('\\' : c : s)	=  '\\' <| c <| lexLitStr s
> lexLitStr (c : s)		=  c <| lexLitStr s

> isSpecial, isSymbol, isIdChar	:: Char -> Bool
> isSpecial c			=  c `elem` ",;()[]{}`"
> isSymbol c			=  (Data.Char.isSymbol c || isPunctuation c) && not (isSpecial c)
> isIdChar c			=  isAlphaNum c || c `elem` "_'"

Reserved identifiers in Haskell.

> {-
> keywords			:: [String]
> keywords			=  [ "case",     "class",    "data",  "default",
>				     "deriving", "do",       "else",  "if",
>				     "import",   "in",       "infix", "infixl",
>				     "infixr",   "instance", "let",   "module",
>				     "newtype",  "of",       "then",  "type",
>				     "where" ]
> -}

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsubsection{Phase 2}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

Join qualified names.

> qualify			:: [Token] -> [Token]
> qualify []			=  []
> qualify (Conid m :  Varsym "." : t@(Conid _) : ts)
>				=  Qual m t : qualify ts
> qualify (Conid m :  Varsym "." : t@(Varid _) : ts)
>				=  Qual m t : qualify ts
> qualify (Conid m : Varsym ('.' : s@(':' : _)) : ts)
>				=  Qual m (Consym s) : qualify ts
> qualify (Conid m : Varsym ('.' : s) : ts)
>				=  Qual m (Varsym s) : qualify ts
> qualify (t : ts)		=  t : qualify ts

Join backquoted identifiers (|tidyup| must be run after |qualify|
since qualified infix operators such as @`Prelude.div`@ are legal).

> tidyup                        :: [Token] -> [Token]
> tidyup []			=  []
> tidyup (Special '`' : t@(Varid _) : Special '`' : ts)
>				=  Op t : tidyup ts
> tidyup (Special '`' : t@(Conid _) : Special '`' : ts)
>				=  Op t : tidyup ts
> tidyup (Special '`' : t@(Qual _ (Varid _)) : Special '`' : ts)
>				=  Op t : tidyup ts
> tidyup (Special '`' : t@(Qual _ (Conid _)) : Special '`' : ts)
>				=  Op t : tidyup ts
> tidyup (String s : ts)	=  strItems s ++ tidyup ts
> tidyup (Space s : ts)		=  splitSpace s ++ tidyup ts
> tidyup (t : ts)		=  t : tidyup ts

NB. @` div `@ will not be joined.

Breaking a string into string items.

> strItems                      :: String -> [Token]
> strItems []			=  impossible "strItems"
> strItems (c : s)		=  case breaks isGap s of
>     (item, '\\' : s')		-> String (c : item ++ "\\") : Space white : strItems rest
>         where (white, rest)	=  span isSpace s'
>     _				-> [String (c : s)]
>
> isGap				:: String -> Bool
> isGap ('\\' : c : _)		=  isSpace c
> isGap _			=  False

> splitSpace                    :: String -> [Token]
> splitSpace []			=  []
> splitSpace s			=  Space t : splitSpace u
>     where (t, u)		=  breakAfter (== '\n') s
