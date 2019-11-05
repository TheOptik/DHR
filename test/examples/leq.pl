:- use_module(library(dhr/test/run_example)).

:- op(400, xfx, leq).

% Program
:- chr_constraint leq/2.
reflexivity  @ X leq X <=> true.
antisymmetry @ X leq Y, Y leq X <=> X = Y.
transitivity @ X leq Y, Y leq Z ==> X leq Z.
idempotence  @ X leq Y \ X leq Y <=> true.

% Tests
leq(1,2), leq(1,2) => leq(1,2).
~ leq(1,2), leq(2,1) => false.
~ leq(1,2), leq(2,3) => leq(1,2), leq(2,3), leq(1,3).
~ leq(a,b), leq(b,c), leq(a,c) => leq(a,b), leq(b,c), leq(a,c).

% Containing variables
leq(A,B), leq(A,B) => leq(A,B).
~ leq(A,B), leq(B,C) => leq(A,B), leq(B,C), leq(A,C).
leq(A,B), leq(B,A) => true.
~ leq(1,2), leq(A,1), leq(2,A) => false.
