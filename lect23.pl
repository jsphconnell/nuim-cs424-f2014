%% -*- prolog -*-

%% Type checker for polymorphic typed lambda calculus
%% written in Prolog.

%% λ Calc                    Prolog term representing it
%%
%% TERMS
%%
%% application:  e1 e2       app(e1,e2)
%%      x y z (a b) d        app(app(app(app(x,y),z),app(a,b)),d)
%% λ-expression: λ v:t . e   lambda(v,t,e)
%% var: v                    v
%% basis element: b          b
%%
%% TYPES
%%
%% prim type: tau            tau
%% function type: t1->t2     arr(t1,t2)
%%   r->(r->r)->r            arr(r,arr(arr(r,r),r))

%% predicates:
%%   typed(e)        % λ-calc expr e is well typed.
%%   oftype(e,t)     % λ-calc expr e has type t

typed(E) :- oftype(E,_).
oftype(app(E1,E2),T2) :- oftype(E1,arr(T1,T2)), oftype(E2,T1).
oftype(lambda(V,T1,E), arr(T1,T2)) :- oftype(E,T2).
oftype(0,r).			% 0 : R
oftype(1,r).			% 1 : R
oftype(sin,arr(r,r)).		% sin : R->R
oftype(plus,arr(r,arr(r,r))).	% plus: R->R->R

%% | ?- oftype(app(plus,1),T).
%% T = arr(r,r)
%% yes
%% | ?- oftype(lambda(v,TV,app(app(plus,1),1)), T).
%% T = arr(TV,r)
%% yes

%% BUG: need to be aware of 
%%  | ?- oftype(lambda(v,TV,v), T).                 
%%  no   % should be T=arr(TV,TV)
%% need to make oftype aware of types of variables in scope.
