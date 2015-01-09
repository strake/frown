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

> module Convert (convert, isNewEOF)
> where

> import qualified Lexer2 as Lex
> import Options
> import Grammar
> import GParser2
> import Stdenv
> --import qualified FiniteMap as FM
> import qualified OrdUniqListSet as Set
> --import OrdUniqListSet         (  Set, MinView(..)  )
> import OrdUniqListSet         (  Set, MinView(Empty, Min)  ) -- for nhc98
> import Atom                   hiding (  string  )
> import Haskell                hiding (  Empty, Decl, guard  )
> import Data.Maybe
> import Data.List
> import Base
> import Prettier               hiding (  concat, intersperse  )
> import qualified Prettier as PP
> import Control.Applicative
> import Control.Monad
> import System.IO                     hiding (  isEOF  )
> import Options

> newEOF                        :: Ident
> newEOF                        =  prime (ident "EOF")

> isNewEOF                      :: Expr -> Bool
> isNewEOF (Con s)              =  s == newEOF
> isNewEOF _                    =  False

> convert                       :: [Flag] -> [Decl] -> IO (Grammar, Int)
> convert opts ds               =  

Check whether terminals and nonterminals are declared.

>     do let verb               =  verbose opts
>
>--        print ds

The previous command is used to dump out the standard environment
(@./frown Stdenv.lg > Stdenv.dump@). The file @Stdenv.dump@ is then
included in the module |Stdenv|. Note that the standard definition
must be appended to the user definitions (since the first symbol
usually determines the start symbol).

>        let decls              =  ds ++ stdenv
>
>        verb "* Checking grammar ..."
>        
>        let dataTerms          =  [ cs | Terminals cs <- decls ]
>        terms                  <- case dataTerms of
>                                      []        -> return []
>                                      _ : _ : _ -> panic "multiple `Terminal' declarations"
>                                      [d]       -> return d
>        let dataNonterms       =  [ cs | Nonterminals cs <- decls ]
>        nonterms               <- case dataNonterms of
>                                      []        -> return []
>                                      _ : _ : _ -> panic "multiple `Nonterminal' declarations"
>                                      [d]       -> return d
>        let nonterms'          =  [ (t, [], b) | (t, b) <- nonterms ] ++ [ (t, ts, b) | TypeSig t ts b <- decls ]

Add |EOF| token if necessary and add for each start symbol |S| a new
start symbol |#S| and a production |#S : S, EOF;| (if the user
declares multiple |EOF| symbols we add one production for each).

>        let hasEOF             =  or [ b | ( _, b, _, _) <- terms ]
>
>        when (Code GVStack `elem` opts && not hasEOF)
>            (panic "explicit EOF symbol required")
>
>        let termsx             =  [ (Con newEOF, True, Unspecified, Nothing) | not hasEOF ] ++ terms
>            terms'             =  [ (s, Terminal i p [] (quotes p) b Copy a s)
>                                  | (i, (p, b, a, s)) <- zip [1 .. ] termsx ]
>
>            (userStarts, rest) =  case partition (\(_, _, b) -> b) nonterms' of
>                                      ([], n : ns) -> ([n], ns) -- no start symbol declared
>                                      (ss, ns)     -> (ss,  ns)
>            newstarts          =  [ ((rename e, hs), ts, True) | ((e, hs), ts, _) <- userStarts ]
>            oldstarts          =  [ (p, ts, False) | (p, ts, _) <- userStarts ]
>            nontermsx          =  newstarts ++ oldstarts ++ rest
>
>            nonterms''         =  [ Nonterminal i (call e) (parameter e ts) [] (map Quoted qs) b
>                                  | (i, ((e, qs), ts, b)) <- zip [length terms' + 1 .. ] nontermsx ]

Add fixity information to the terminals.

>        let Writer fixities vs =  sequence [ termLookup t terms' >>= \ v -> return (v, a)
>                                           | Fixity a t <- decls ]
>        when (not (null vs))
>            (panic (render OneLine (string "undefined symbols: " <> prettySymbols vs <> string ".")))
>
>        let terms''            =  [ (s, case lookup t fixities of Nothing -> t; Just a -> t { assoc = a})
>                                  | (s, t) <- terms' ]

Converting the productions.

>            lhs (e, qs)        =  lhsLookup (e, length qs) nonterms'' >>= \ v -> return (v{ attributes = map Quoted qs })
>            rhs _ps (m, Term p)=  termLookup  p terms'' >>= \ v -> return (v{ attributes = quotes p, modifier = m })
>            rhs ps (_m, Nonterm (e, qs))
>                               =  nontermLookup (e, length qs) terms'' (ps ++ nonterms'') >>= \ v -> return (v{ attributes = map Quoted qs })
>
>            Writer userRules vs=  sequence [ do w'  <- lhs w
>                                                ws' <- mapM (rhs (arguments w')) ws
>                                                return (w', ws')
>                                           | Production w ws <- decls ]

Check whether there are undefined symbols.

>        when (not (null vs))
>            (panic (render OneLine (string "undefined symbols: " <> prettySymbols vs <> string ".")))
>
>        let annotate v         =  v { attributes = [ var ("v" ++ show k) | k <- [1 .. length (types v)] ]}
>
>            newstarts'         =  take (length newstarts) nonterms'' -- for |rules|
>            oldstarts'         =  drop (length newstarts) nonterms'' -- for |rules|
>            rules              =  [ (annotate s, [annotate s', t])
>                                  | (s, s') <- zip newstarts' oldstarts', (_, t) <- terms'', isEOF t  ]
>                               ++ userRules

Expand rule schemes.

>        let expand             :: Set Symbol -> Set Symbol -> [(Symbol, [Symbol])]
>            expand done todo   =  case Set.minview todo of
>              Empty            -> []
>              Min v todo'      -> rs ++ expand done' (Set.union todo' new)
>                where rs       =  [ ruleInstance v r | r <- rules, name (fst r) == name v && arity (fst r) == arity v ]
>                      done'    =  Set.add v done
>                      new      =  Set.fromList [ w | (_, ws) <- rs, w@(Nonterminal {}) <- ws ] `Set.minus` done'
>
>            starts             =  take (length newstarts) nonterms''
>
>            expandedRules      =  expand Set.empty (Set.fromList starts)
>
>            g                  =  Grammar
>                                  { terminals    =  map snd terms''
>                                  , nonterminals =  Set.toList (Set.fromList (map fst expandedRules)) -- nonterms''
>                                  , startSymbols =  starts
>                                  , productions  =  [ Rule i n (removePrec r) (prec (precedence r))
>                                                    | (i, (n, r)) <- zip [1 ..] expandedRules ]
>                                  }
>
>            m                  =  maximum [ length (rrhs r) | r <- productions g ]
>
>        verb ("  " ++ show (length (terminals g)) ++ " terminals")
>        verb ("  " ++ show (length (nonterminals g)) ++ " nonterminals")
>        verb ("  " ++ show (length (startSymbols g)) ++ " start symbols")
>        verb ("  " ++ show (length (productions g)) ++ " productions")
>        verb ("  " ++ show (length [ n | n <- nonterminals g, let ps = productionsOf g n, length ps == 1 && length (rrhs (head ps)) == 1 ]) ++ " single productions")
>
>--        print g
>        return (g, m)

> prettySymbols                 :: [(Expr, Int)] -> Doc
> prettySymbols vs              =  PP.intersperse (string "," <> nl)
>                                    [ pretty v <> string "/" <> pretty n <> string " at " <> pretty (location v) | (v, n) <- vs ]

> location (App e _)            =  location e
> location (Con i)              =  identSrcLoc i
> location (Var i)              =  identSrcLoc i
> location (Literal l)          =  litSrcLoc l
> location _                    =  Unknown

> removePrec                    :: [Symbol] -> [Symbol]
> removePrec r                  =  [ v | v <- r, not (terminal v) || modifier v /= Prec ]
>
> precedence                    :: [Symbol] -> Assoc
> precedence r                  =  head ([ assoc v | v <- reverse r, terminal v ] ++ [Unspecified])

> {-
> legal                         :: Expr -> Bool
> legal (Con _k)                =  True
> legal (App p (Quoted _ts))    =  legal p
> legal _                       =  False
> -}

> rename                        :: Expr -> Expr
> rename (Var i)                =  Var (prime i)
> rename (App p p')             =  App (rename p) p'
> rename _                      =  impossible "Convert.rename"

> call                          :: Expr -> Ident
> call (Var s)                  =  s
> call (App p _p')              =  call p
> call _                        =  impossible "Convert.call"

> isCall                        :: Expr -> Bool
> isCall (Var s)                =  True
> isCall (App p _p')            =  isCall p
> isCall _                      =  False

> parameter                     :: Expr -> [Nonterm] -> [Symbol]
> parameter e vs                =  [ Nonterminal n s [] [] (typeLookup s vs) False
>                                  | (n, Var s) <- zip [-1, -2 ..] (args e) ] -- HACK: parameters are assigned negative numbers

> args                          :: Expr -> [Expr]
> args (App p e)                =  args p ++ [e]
> args _                        =  []

> typeLookup                    :: Ident -> [Nonterm] -> [Type]
> typeLookup s vs               =  head [ map Quoted qs | (Var s', qs) <- vs, s == s' ]

Writer monad for collecting multiple error messages.

> data Writer w a               =  Writer a [w]

> instance Functor (Writer w) where
>     f `fmap` Writer a ss      = Writer (f a) ss

> instance Applicative (Writer w) where
>     pure                      = flip Writer []
>     Writer f ss <*> Writer x ts = Writer (f x) (ss ++ ts)

> instance Monad (Writer w) where
>     return a                  =  Writer a []
>     Writer a ss >>= k         =  let Writer b ss' = k a in Writer b (ss ++ ss')

> write                         :: w -> Writer w ()
> write s                       =  Writer () [s]

\begin{THIS IS A MESS!}

> type XXX a                    =  Writer (Expr, Int) a

Looking up symbols. If |p| is a string literal, then it might be an
abbreviation.

> isShortcut (Literal s)        =  True
> isShortcut (App p _p')        =  isShortcut p
> isShortcut _                  =  False

> theShortcut (Literal s)       =  s
> theShortcut (App p _p')       =  theShortcut p
> theShortcut _                 =  impossible "Convert.theShortcut"

> arity                         :: Symbol -> Int
> arity v                       =  length (types v)

> termLookup                    :: Expr -> [(Maybe Literal, Symbol)] -> XXX Symbol
> termLookup p fm
>     | isShortcut p            =  case lookup (theShortcut p, length (args p)) [ ((s', arity v), v) | (Just s', v) <- fm ] of
>                                      Nothing -> termLookup' p (map snd fm)
>                                      Just v  -> return v
>     | otherwise               =  termLookup' p (map snd fm)
>
> termLookup'                   :: Expr -> [Symbol] -> XXX Symbol
> termLookup' p fm              =  case msum (map (matches p) fm) of
>                                      Nothing -> write (p, length (quotes p)) >> return undef
>                                      Just v  -> return v

> nontermLookup                 :: (Expr, Int) -> [(Maybe Literal, Symbol)] -> [Symbol] -> XXX Symbol
> nontermLookup e ts nts        =  do v  <- safeLookup e [ ((name v, arity v), v) | v <- nts ]
>                                     vs <- sequence [ nontermLookup' (a, arity arg) ts nts | (a, arg) <- zip (args (fst e)) (arguments v) ] -- args (fst e) ]
>                                     return (updateArgs v vs)

> nontermLookup'                :: (Expr, Int) -> [(Maybe Literal, Symbol)] -> [Symbol] -> XXX Symbol
> nontermLookup' e ts nts
>     | isCall (fst e)          =  nontermLookup e ts nts
>     | otherwise               =  xtermLookup e ts

> xtermLookup                   :: (Expr, Int) -> [(Maybe Literal, Symbol)] -> XXX Symbol
> xtermLookup (p, n) fm
>     | isShortcut p            =  case lookup (theShortcut p, n) [ ((s', arity v), v) | (Just s', v) <- fm ] of
>                                      Nothing -> termLookup' p (map snd fm)
>                                      Just v  -> return v
>     | otherwise               =  termLookup' p (map snd fm)
>
> updateArgs v vs               =  v { arguments = vs, types = map (subst tenv) (types v) }
>   where tenv                  =  [ (tyvar t, t')
>                                  | (w, w') <- zip (arguments v) vs
>                                  , (t, t') <- zip (types w) (types w') ]

> tyvar                         :: Type -> String
> tyvar (Quoted qs)             =  case filter (not . Lex.isWhite) qs of
>                                      [Lex.Varid s] -> s
>                                      _             -> error "type variable expected"

> subst                         :: [(String, Type)] -> Type -> Type
> subst env (Quoted qs)         =  Quoted (concatMap sub qs)
>     where sub (Lex.Varid s)   =  case lookup s env of
>                                      Nothing           -> [Lex.Varid s]
>                                      Just (Quoted qs') -> [Lex.LeftParen] ++ qs' ++ [Lex.RightParen]
>           sub t               =  [t]

> lhsLookup                     :: (Expr, Int) -> [Symbol] -> XXX Symbol
> lhsLookup e fm                =  do v <- safeLookup e [ ((name v, arity v), v) | v <- fm ]
>                                     return (v { arguments = [ w { name = s }
>                                                             | (Var s, w) <- zip (args (fst e)) (arguments v)] })

> safeLookup                    :: (Expr, Int) -> [((Ident, Int), Symbol)] -> XXX Symbol
> safeLookup (e, n) fm          =  case lookup (call e, n) fm of
>                                      Nothing -> write (e, n) >> return undef
>                                      Just v  -> return v
>
> undef                         :: Symbol
> undef                         =  Nonterminal 0 (ident "undef") [] [] [] False

\end{THIS IS A MESS!}

Pattern matching.

For the moment being we require the declared and the actual pattern to
be identical, except for the quoted components.

> matches                       :: Expr -> Symbol -> Maybe Symbol
> matches p (v@(Terminal { pattern = p' }))
>                               =  do q <- match p p'
>                                     return (v{pattern = q})
> matches _ _                   =  impossible "Convert.matches"

> match                         :: Expr -> Expr -> Maybe Expr
> match (Var s) (Var s')
>     | s == s'                 =  return (Var s)
>     | otherwise               =  fail "match"
> match (Con s) (Con s')
>     | s == s'                 =  return (Con s)
>     | otherwise               =  fail "match"
> match (Literal s) (Literal s')
>     | s == s'                 =  return (Literal s)
>     | otherwise               =  fail "match"
> match (Tuple ps) (Tuple ps')  =  do guard (length ps == length ps')
>                                     qs <- zipWithM match ps ps'
>                                     return (Tuple qs)
> match (List ps) (List ps')    =  do guard (length ps == length ps')
>                                     qs <- zipWithM match ps ps'
>                                     return (List qs)
> match (App p q) (App p' q')   =  do r <- match p p'
>                                     r' <- match q q'
>                                     return (App r r')
> match (Quoted as) (Quoted ts) =  return (TypeOf as ts)
> match _ _                     =  fail "match"

Instance of a rule scheme.

> ruleInstance                  :: Symbol -> (Symbol, [Symbol]) -> (Symbol, [Symbol])
> ruleInstance v (lhs, rhs)     =  (substitute lhs, [ substitute w | w <- rhs])
>   where bindings              =  zip (arguments lhs) (arguments v)
>         substitute w
>           | terminal w        =  w
>           | otherwise         =  case lookup w bindings of
>                                      Just w' -> w' { attributes = attributes w }
>                                      Nothing -> updateArgs w (map substitute (arguments w))