-----------------------------------------------------------------------------
--
-- (c) The GHC Team 1997-2000
--
-- Utilities for the Haskell parser.
--
-----------------------------------------------------------------------------

ToDo: Polish readInteger, readRational

> module HsParseUtils (
> 	  splitTyConApp		-- HsType -> Lex (HsName,[HsType])
> 	, mkModule 		-- Module -> Maybe [HsExportSpec] 
> 			--   -> ([HsImportDecl],[HsDecl]) 
> 			--   -> HsModule
> 	, mkRecConstrOrUpdate	-- HsExp -> [HsFieldUpdate] -> Lex HsExp
> 	, checkPrec 		-- String -> Lex String
> 	, checkContext		-- HsType -> Lex HsContext
> 	, checkAssertion	-- HsType -> Lex HsAsst
> 	, checkDataHeader	-- HsQualType -> Lex (HsContext,HsName,[HsName])
> 	, checkSimple		-- HsType -> [HsName] -> Lex ((HsName,[HsName]))
> 	, checkPattern		-- HsExp -> Lex HsPat
> 	, checkPatterns		-- [HsExp] -> Lex [HsPat]
> 	, checkExpr		-- HsExp -> Lex HsExp
> 	, checkValDef		-- (SrcLoc, HsExp, HsRhs, [HsDecl]) -> Lex HsDecl
> 	, checkUnQual		-- HsQName -> Lex HsName
> 	, readInteger 		-- String -> Integer
> 	, readRational 		-- String -> Rational
>  ) where
> 
> import HsSyn
> import HsParseMonad
> 
> import Char(isDigit,isOctDigit,isHexDigit,digitToInt)
> import Ratio

> splitTyConApp :: HsType -> Lex (HsName,[HsType])
> splitTyConApp t = split t []
>  where
> 	split :: HsType -> [HsType] -> Lex (HsName,[HsType])
> 	split (HsTyApp t u) ts = split t (u:ts)
> 	split (HsTyCon (UnQual t)) ts = return (t,ts)
> 	split _ _ = parseError "Illegal data/newtype declaration"

Various Syntactic Checks.

> checkContext :: HsType -> Lex HsContext
> checkContext (HsTyTuple ts) = 
>      mapM checkAssertion ts >>= \cs ->
>      return cs
> checkContext t = 
>      checkAssertion t >>= \c ->
>      return [c]

Changed for multi-parameter type classes.

> checkAssertion :: HsType -> Lex HsAsst
> checkAssertion = checkAssertion' []
> 	where	checkAssertion' ts (HsTyCon c) = return (c,ts)
> 		checkAssertion' ts (HsTyApp a t) = checkAssertion' (t:ts) a
> 		checkAssertion' _ _ = parseError "Illegal class assertion"
> 
> 
> checkDataHeader :: HsQualType -> Lex (HsContext,HsName,[HsName])
> checkDataHeader (HsQualType cs t) =
>    checkSimple t []	     >>= \(c,ts) ->
>    return (cs,c,ts)
> checkDataHeader (HsUnQualType t) =
>    checkSimple t []	     >>= \(c,ts) ->
>    return ([],c,ts)
> 
> checkSimple :: HsType -> [HsName] -> Lex ((HsName,[HsName]))
> checkSimple (HsTyApp l (HsTyVar a)) xs = checkSimple l (a:xs)
> checkSimple (HsTyCon (UnQual t))    xs = return (t,xs)
> checkSimple _ _ = parseError "Illegal data/newtype declaration"


Checking Patterns.

We parse patterns as expressions and check for valid patterns below,
converting the expression into a pattern at the same time.

> checkPattern :: HsExp -> Lex HsPat
> checkPattern e = checkPat e []
> 
> checkPatterns :: [HsExp] -> Lex [HsPat]
> checkPatterns es = mapM checkPattern es
> 
> checkPat :: HsExp -> [HsPat] -> Lex HsPat
> checkPat (HsCon c) args = return (HsPApp c args)
> checkPat (HsApp f x) args = checkPat x [] >>= \x -> checkPat f (x:args)
> checkPat e [] = case e of
> 	HsVar (UnQual x)   -> return (HsPVar x)
> 	HsLit l            -> return (HsPLit l)
> 	HsInfixApp l op r  -> checkPat l [] >>= \l ->
> 			      checkPat r [] >>= \r ->
> 			      case op of
> 				 HsCon c -> return (HsPInfixApp l c r)
> 				 _ -> patFail
> 	HsTuple es         -> mapM (\e -> checkPat e []) es >>= \ps ->
> 			      return (HsPTuple ps)
> 	HsList es	   -> mapM (\e -> checkPat e []) es >>= \ps ->
> 			      return (HsPList ps)
> 	HsParen e	   -> checkPat e [] >>= (return . HsPParen)
> 	HsAsPat n e	   -> checkPat e [] >>= (return . HsPAsPat n)
> 	HsWildCard	   -> return HsPWildCard
> 	HsIrrPat e	   -> checkPat e [] >>= (return . HsPIrrPat)
> 	HsRecConstr c fs   -> mapM checkPatField fs >>= \fs ->
> 			      return (HsPRec c fs)
> 	HsNegApp (HsLit l) -> return (HsPNeg (HsPLit l))
> 	_ -> patFail
> 
> checkPat _ _ = patFail
> 
> checkPatField :: HsFieldUpdate -> Lex HsPatField
> checkPatField (HsFieldUpdate n e) = 
>    checkPat e [] >>= \p ->return (HsPFieldPat n p)
> 
> patFail = parseError "Parse error in pattern"


Check Expression Syntax.

> checkExpr :: HsExp -> Lex HsExp
> checkExpr e = case e of
> 	HsVar _			  -> return e
> 	HsCon _			  -> return e
> 	HsLit _			  -> return e
> 	HsInfixApp e1 e2 e3	  -> check3Exprs e1 e2 e3 HsInfixApp
> 	HsApp e1 e2		  -> check2Exprs e1 e2 HsApp
> 	HsNegApp e		  -> check1Expr e HsNegApp
> 	HsLambda ps e		  -> check1Expr e (HsLambda ps)
> 	HsLet bs e		  -> check1Expr e (HsLet bs)
> 	HsIf e1 e2 e3		  -> check3Exprs e1 e2 e3 HsIf
> 	HsCase e alts		  -> mapM checkAlt alts >>= \alts ->
> 				     checkExpr e >>= \e ->
> 				     return (HsCase e alts)
> 	HsDo stmts		  -> mapM checkStmt stmts >>= (return . HsDo)
> 	HsTuple es		  -> checkManyExprs es HsTuple
> 	HsList es		  -> checkManyExprs es HsList
> 	HsParen e		  -> check1Expr e HsParen
> 	HsLeftSection e1 e2	  -> check2Exprs e1 e2 HsLeftSection
> 	HsRightSection e1 e2      -> check2Exprs e1 e2 HsRightSection
> 	HsRecConstr c fields	  -> mapM checkField fields >>= \fields ->
> 				     return (HsRecConstr c fields)
> 	HsRecUpdate e fields	  -> mapM checkField fields >>= \fields ->
> 				     checkExpr e >>= \e ->
> 				     return (HsRecUpdate e fields)
> 	HsEnumFrom e		  -> check1Expr e HsEnumFrom
> 	HsEnumFromTo e1 e2	  -> check2Exprs e1 e2 HsEnumFromTo
> 	HsEnumFromThen e1 e2      -> check2Exprs e1 e2 HsEnumFromThen
> 	HsEnumFromThenTo e1 e2 e3 -> check3Exprs e1 e2 e3 HsEnumFromThenTo
> 	HsListComp e stmts        -> mapM checkStmt stmts >>= \stmts ->
> 				     checkExpr e >>= \e ->
> 				     return (HsListComp e stmts)
> 	HsExpTypeSig loc e ty     -> checkExpr e >>= \e ->
> 				     return (HsExpTypeSig loc e ty)
> 	_                         -> parseError "parse error in expression"

Type signature for polymorphic recursion!!

> check1Expr :: HsExp -> (HsExp -> a) -> Lex a
> check1Expr e f = checkExpr e >>= (return . f)
> 
> check2Exprs :: HsExp -> HsExp -> (HsExp -> HsExp -> a) -> Lex a
> check2Exprs e1 e2 f = 
> 	checkExpr e1 >>= \e1 ->
> 	checkExpr e2 >>= \e2 ->
> 	return (f e1 e2)
> 
> check3Exprs :: HsExp -> HsExp -> HsExp -> (HsExp -> HsExp -> HsExp -> a) -> Lex a
> check3Exprs e1 e2 e3 f = 
> 	checkExpr e1 >>= \e1 ->
> 	checkExpr e2 >>= \e2 ->
> 	checkExpr e3 >>= \e3 ->
> 	return (f e1 e2 e3)
> 
> checkManyExprs es f =
> 	mapM checkExpr es >>= \es ->
> 	return (f es) 
> 
> checkAlt (HsAlt loc p galts bs) 
> 	= checkGAlts galts >>= \galts -> return (HsAlt loc p galts bs)
> 
> checkGAlts (HsUnGuardedAlt e) = check1Expr e HsUnGuardedAlt
> checkGAlts (HsGuardedAlts galts) 
>     	= mapM checkGAlt galts >>= (return . HsGuardedAlts)
> 
> checkGAlt (HsGuardedAlt loc e1 e2) = check2Exprs e1 e2 (HsGuardedAlt loc)
> 
> checkStmt (HsGenerator p e) = check1Expr e (HsGenerator p)
> checkStmt (HsQualifier e)   = check1Expr e HsQualifier
> checkStmt s@(HsLetStmt bs)  = return s
> 
> checkField (HsFieldUpdate n e) = check1Expr e (HsFieldUpdate n)

Check Equation Syntax.

> checkValDef :: (SrcLoc, HsExp, HsRhs, [HsDecl]) -> Lex HsDecl
> checkValDef (srcloc, lhs, rhs, whereBinds) =
>     case isFunLhs lhs [] of
>  	 Just (f,es) -> checkPatterns es >>= \ps ->
> 	          	return (HsFunBind srcloc 
> 			[HsMatch srcloc f ps rhs whereBinds])
>        Nothing     -> checkPattern lhs >>= \lhs ->
> 		  	return (HsPatBind srcloc lhs rhs whereBinds)

A variable binding is parsed as an HsPatBind.

> isFunLhs (HsInfixApp l (HsVar op) r) es = Just (op, l:r:es)
> isFunLhs (HsApp (HsVar f) e) es = Just (f,e:es)
> isFunLhs (HsApp (HsParen f) e) es = isFunLhs f (e:es)
> isFunLhs (HsApp f e) es = isFunLhs f (e:es)
> isFunLhs _ _ = Nothing

Check that an identifier or symbol is unqualified. For occasions when
doing this in the grammar would cause conflicts.

> checkUnQual :: HsQName -> Lex HsName
> checkUnQual (Qual _ _) = parseError "Illegal qualified name"
> checkUnQual (UnQual n) = return n

Miscellaneous utilities.

> checkPrec :: String -> Lex String
> checkPrec cs@[c] | isDigit c = return cs
> checkPrec cs                 = parseError ("Illegal precedence " ++ cs)

Stolen from Hugs' Prelude.

> readInteger :: String -> Integer
> readInteger ('0':'o':ds) = readInteger2  8 isOctDigit ds
> readInteger ('0':'x':ds) = readInteger2 16 isHexDigit ds
> readInteger          ds  = readInteger2 10 isDigit    ds
> 
> readInteger2 :: Integer -> (Char -> Bool) -> String -> Integer
> readInteger2 radix isDig ds 
>   = foldl1 (\n d -> n * radix + d) (map (fromIntegral . digitToInt) ds)

Hack ...

> readRational :: String -> Rational
> readRational xs = (readInteger (i++m))%1 * 10^^(case e of {[] -> 0;  ('+':e2) -> read e2; _ -> read e} - length m)
>   where (i,r1) = span isDigit xs
>         (m,r2) = span isDigit (dropWhile (=='.') r1)
>         e      = dropWhile (=='e') r2
> 
> mkModule 
>   	:: Module 
> 	-> Maybe [HsExportSpec] 
> 	-> ([HsImportDecl],[HsDecl]) 
> 	-> HsModule
> 
> mkModule m e (imp, d) 
>    = HsModule m e (reverse imp) (reverse d)
> 
> mkRecConstrOrUpdate :: HsExp -> [HsFieldUpdate] -> Lex HsExp
> mkRecConstrOrUpdate (HsCon c) fs       = return (HsRecConstr c fs)
> mkRecConstrOrUpdate exp       fs@(_:_) = return (HsRecUpdate exp fs)
> mkRecConstrOrUpdate _         _        = parseError "Empty record update"