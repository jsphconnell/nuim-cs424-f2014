%% -*- prolog -*-

%% (I) Prolog Execution Model

% foo(...) :- bar(...), baz(...).    % rule 1 (about foo)
% foo(...) :- zonk(...), zink(...).  % rule 2 (about foo)

%% trying to prove: foo(xxx)
%%  rules are an "OR", can prove foo(xxx) using EITHER rule 1 or rule 2.
%%  stuff on right
%%       head(xxx) :- tail1(yyy), tail2(zzz), tail3(whatever).
%%       ^HEAD....    ^....................TAIL...............
%%  i.e., the terms in the tail of a rule are an AND.
%% SEARCH goes top-to-bottom on rules (OR),
%%             left to right on terms (AND).

%% If (lect 21, family tree) instead of this:
% father(X,Y) :- parent(X,Y), male(X).      % option 1
%% we had this:
% father(X,Y) :- male(X), parent(X,Y).      % option 2
%% and try to prove
% father(X,nili)
%% option 1: fast, find each parent, check if that person is male.
%% option 2: slow, find each male, check if that person is parent

%% If you documentation says:
%%  foo(+,+,-)
%% means can call foo w/ first two args filled in, will figure out 3rd.
%%  foo(+,+,-)
%%  foo(-,-,+)
%% together, means can EITHER fill in 1st&2nd args, or 3rd arg.

%% (II) Peano Arithmetic

%% represent natural numbers as:
%%   z  (for zero)
%%   s(X)  (for successor of X)

% equal(z,z).
% equal(s(X),s(Y)) :- equal(X,Y).
%% wait, can just use
% equal(X,X).
% aka X=X.

%% sum(A,B,C) means the sum of A and B is C.
sum(z,B,B).
sum(s(A),B,s(C)) :- sum(A,B,C).

%% subtract(A,B,C) means A-B=C
subtract(A,B,C) :- sum(B,C,A).

%% mult(A,B,C) means A*B=C
mult(z,_,z).
mult(s(A),B,C) :- mult(A,B,AB), sum(AB,B,C).

%% ge(A,B) means A>=B
ge(_,z).
ge(s(X),s(Y)) :- ge(X,Y).

composite(X) :- mult(A,B,X), ge(A, s(s(z))), ge(B, s(s(z))).

%% problem: fix so
%%  mult(X,Y,s(s(s(s(z)))))
%% terminates.
