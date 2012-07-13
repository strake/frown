{-

This grammar is LALR(1) but not SLR(1) (see dragon book p229).

	frown --debug LValue.g

-}

module LValue
where

type Result                   =  Maybe

%{

Terminal                      =  Eq | Star | Id;
Nonterminal                   =  stat | lvalue | rvalue;

stat                          :  lvalue, Eq, rvalue;
                              |  rvalue;

lvalue                        :  Star, rvalue;
                              |  Id;

rvalue                        :  lvalue;

}%

data Terminal                 =  Eq | Star | Id

frown _                       =  fail "syntax error"
