






























 module Optimize              (  optimize  )
 where
 import Grammar
 import LR0
 import Lookahead
 import Haskell
 import qualified SearchTree as FM
 import Base
 import Maybe                  (  fromMaybe  )








 optimize                      :: ActionTable -> ActionTable
 optimize table                =  fmap (map opt) table
     where
     lookup s                  =  applyWithDefault (FM.lookup table) [] s

     opt a@(Shift _)           =  a
     opt a@(Reduce{ goto = e })=  a{ goto = fromMaybe e (peval e) }

     peval e@(_, n, s)         =  case [ a | a <- lookup s, match e a ] of
                                      [Reduce{ stack = Nil :> (_, n', _), goto = e' }]
                                          -> Just (s0, n1 { pattern = compose n n' n1}, s1)
                                             where (s0, n1, s1) =  fromMaybe e' (peval e')
                                      _ ->  Nothing

 match                         :: Edge -> Action -> Bool
 match _e (Shift _)            =  True
 match _e (Reduce{ stack = Nil })
                               =  True
 match e (Reduce{ stack = _ :> e' })
                               =  e == e'

 compose                       :: Symbol -> Symbol -> Symbol -> Expr
 compose e p e'                =  Case (Tuple (argsOf e))
                                      [(Tuple (argsOf p), pattern e')]


 argsOf                        =  map fst . quotesOf . pattern


