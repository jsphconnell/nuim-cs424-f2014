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
%%      API: typed/1(+)
%%   oftype(e,env,t) % λ-calc expr e has type t in type environment env
%%      API: oftype(+,+,-)
%% type environment: list of variable/type lists.
%% E.g., [[x,r],[y,r],[z,arr(r,r)]]

typed(E) :- oftype(E,[],_).

%% types of basis symbols 
oftype(0,_,r).			% 0 : R
oftype(1,_,r).			% 1 : R
oftype(sin,_,arr(r,r)).		% sin : R->R
oftype(plus,_,arr(r,arr(r,r))).	% plus: R->R->R

%% type of an application
oftype(app(E1,E2), Env, T2) :-
	oftype(E1, Env, arr(T1,T2)),
	oftype(E2, Env, T1).

%% type of a lambda expression
oftype(lambda(V,T1,Body), Env, arr(T1,T2)) :-
	oftype(Body, [[V,T1]|Env], T2).

%% type of a variable
oftype(V,Env,T) :- member([V,T],Env).

%% | ?- oftype(app(plus,1),T).
%% T = arr(r,r)

%% | ?- oftype(lambda(v,TV,app(app(plus,1),1)), T).
%% T = arr(TV,r)

%% | ?- oftype(lambda(v,TV,v), [], T).
%% T = arr(TV,TV) ? ;
