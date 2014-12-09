%% -*- prolog -*-

%% Prolog = PROgramming in LOGic

%% Program = set of axioms
%% Computation = Proving a theorem using those axioms
%%               ^Constructively

%% syntax: Predicate Calculus
%% semantics: Resolution Theorem Proving

man(aristotle).			% read as: aristotle is a man
mortal(X) :- man(X).		% read as:
				%    for all X, man(X) -> mortal(X)
				% or
				%    X is mortal if X is a man
% HEAD :- ...
% "man(aristotle)" is a "term"

%% capitalized = variable
%% lower case = constant

%% to load this file in SWI Prolog:
%%% consult(lect21).

%% Prolog's "database" constants facts and rules.

%% Facts:
male(barak).
male(fishel).
parent(fishel,barak).
parent(jacob,fishel).
male(jacob).
parent(toba,fishel).
female(toba).

%% Rules:
father(X,Y) :- parent(X,Y), male(X).
sibling(X,Y) :- parent(Z,X), parent(Z,Y).
grandparent(X,Y) :- parent(X,Z), parent(Z,Y).
grandfather(X,Y) :- grandparent(X,Y), male(X).

%% Home brew lists
%% empty list = nil
%% list whose 1st element is X and remaining elts are Xs: cons(X,Xs)

%% list of [aye,bee,sea] = cons(aye,cons(bee,cons(sea,nil)))

%% _ is anonymous unused variable
membr(X,cons(X,_)).		
membr(X,cons(_,Ys)) :- membr(X,Ys).

%% read: does there exist an X such that ...
% ?- membr(X,cons(aye,cons(bee,cons(sea,nil)))).    <- "query"
% X = aye ;
% X = bee ;
% X = sea ;
% false.

%% Prolog uses term: .(a,.(b,.(c,[])))
%% for list [a,b,c].
%% [a,b,c] is syntactic sugar for: .(a,.(b,.(c,[]))).
%% [a,b,c|Xs] is syntactic sugar for .(a,.(b,.(c,Xs))).

membrr(X,[X|_]).
membrr(X,[_|Ys]) :- membrr(X,Ys).


%% app(Xs,Ys,Zs) mean list Xs appended to Ys is the list Zs.
app([],L,L).
app([X|Xs],Ys,[X|Zs]) :- app(Xs,Ys,Zs).

membrrr(X,L) :- app(_,[X|_],L).

%% predefined:
%%  =(X,X).
%% predefined:
%%  member/2, append/3
