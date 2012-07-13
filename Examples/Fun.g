{-

An ambiguous grammar (3 reduce/reduce conflicts).

	frown --backtrack Fun.g

-}

module Fun
where
import Monad

type Result                   =  []

%{

Terminal                      =  Special ';' | Special ',' | Special '=' | Special '|' | Var;
Nonterminal                   =  bindings | binding | alts | expr | vars | fun;

bindings                      :  bindings, binding;
                              |  ;

binding                       :  fun, vars, Special '=', expr, Special ';', alts;

alts                          :  vars, Special '|', expr, Special ';', alts;
                              |  ;

expr                          :  Var;
                              |  fun, vars;

vars                          :  vars, Var;
                              |  ;

fun                           :  Var;

}%

data Terminal                 =  Special Char | Var

frown _                       =  fail "syntax error"