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
\section{The type of tokens}
%-------------------------------=  --------------------------------------------

> module Lexer2
> where
> import Prelude
> import Data.Char
> import System.IO
> import Control.Monad
> import Base
> import Options

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{The |Token| type}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> data Token			=  White   String
>				|  Comment String
>				|  Nested  String
>				|  Conid   String
>				|  Varid   String
>				|  Consym  String
>				|  Varsym  String
>				|  Numeral String
>				|  Char    String
>				|  String  String
>                               |  Comma
>                               |  Semicolon
>                               |  LeftParen
>                               |  RightParen
>                               |  LeftBracket
>                               |  RightBracket
>                               |  LeftCurly
>                               |  RightCurly
>                               |  Backquote
>                               |  LeftSpecial
>                               |  RightSpecial
>                               |  EOF
>				   deriving (Eq, Ord, Show)

> isVarid, isConid, isWhite	:: Token -> Bool
> isVarid (Varid _)		=  True
> isVarid _			=  False
>
> isConid (Conid _)		=  True
> isConid _			=  False
>
> isWhite (White _)		=  True
> isWhite (Comment _)	        =  True
> isWhite (Nested _)	        =  True
> isWhite _			=  False

> toString			:: Token -> String
> toString (White s)		=  s
> toString (Comment s)		=  "--" ++ s
> toString (Nested s)		=  "{-" ++ s ++ "-}"
> toString (Conid s)		=  s
> toString (Varid s)		=  s
> toString (Consym s)		=  s
> toString (Varsym s)		=  s
> toString (Numeral s)		=  s
> toString (Char s)		=  s
> toString (String s)		=  s
> toString Comma		=  ","
> toString Semicolon		=  ";"
> toString LeftParen		=  "("
> toString RightParen		=  ")"
> toString LeftBracket		=  "["
> toString RightBracket		=  "]"
> toString LeftCurly		=  "{"
> toString RightCurly		=  "}"
> toString Backquote		=  "`"
> toString LeftSpecial		=  "%{"
> toString RightSpecial		=  "}%"
> toString EOF                  =  "<end of input>"

% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -
\subsection{Some helper functions}
% - - - - - - - - - - - - - - - = - - - - - - - - - - - - - - - - - - - - - - -

> lexFracExp			:: String -> Maybe (String, String)
> lexFracExp s			=  do t <- match "." s
>				      (ds, u) <- lexDigits' t
>				      (e, v)  <- lexExp u
>				      return ('.' : ds ++ e, v)
>				`mplus` return ("", s)
>
> lexExp			:: String -> Maybe (String, String)
> lexExp (e : s)
>      | e `elem` "eE" 		=  do (c : t) <- return s
>				      if c `elem` "+-" then return () else mzero
>				      (ds, u) <- lexDigits' t
>				      return (e : c : ds, u)
>				`mplus` do (ds, t) <- lexDigits' s
>				           return (e : ds, t)
> lexExp s			=  return ("", s)
>
> lexDigits'			:: String -> Maybe (String, String)
> lexDigits' s			=  do (cs@(_ : _), t) <- return (span isDigit s)
>                                     return (cs, t)

> match				:: String -> String -> Maybe String
> match p s
>     | p == t			=  Just u
>     | otherwise		=  Nothing
>     where (t, u)		=  splitAt (length p) s

> nested			:: Int -> String -> (String, String)
> nested _     []		=  ([], [])
> nested 0     ('-' : '}' : s)	=  ([], '-':'}':s)
> nested n     ('-' : '}' : s)	=  '-' <| '}' <| nested (n - 1) s
> nested n     ('{' : '-' : s)	=  '{' <| '-' <| nested (n + 1) s
> nested n     (c : s)		=  c <| nested n s

> lexCharLit, lexStrLit		:: String -> (String, String)
> lexCharLit []			=  ([], [])
> lexCharLit ('\'' : s)		=  ([], '\'' : s)
> lexCharLit ('\\' : c : s)	=  '\\' <| c <| lexCharLit s
> lexCharLit (c : s)		=  c <| lexCharLit s
>
> lexStrLit []			=  ([], [])
> lexStrLit ('"' : s)		=  ([], '"' : s)
> lexStrLit ('\\' : c : s)	=  '\\' <| c <| lexStrLit s
> lexStrLit (c : s)		=  c <| lexStrLit s

> isSymbol, isIdChar	        :: Char -> Bool
> isSymbol c			=  (Data.Char.isSymbol c || isPunctuation c) && not (c `elem` "()[]{};,`")
> isIdChar c			=  isAlphaNum c || c `elem` "_'"
