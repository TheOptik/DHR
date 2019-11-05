:- use_module(library(dhr/test/run_example)).

% Program
:- chr_constraint succ_h/2, upto_h/1, hamming/1.

succ_h(A, A) <=> true.
succ_h(A, B) \ succ_h(A, C) <=> A < B, B =< C | succ_h(B, C).

upto_h(N), succ_h(S, X) \ hamming(S) <=> X < N |
    X2 is X*2, succ_h(X, X2),
    X3 is X*3, succ_h(X, X3),
    X5 is X*5, succ_h(X, X5),
    hamming(X).

% Tests
~ succ_h(0, 1), hamming(0), upto_h(1) => upto_h(1), hamming(0), succ_h(0, 1).
~ succ_h(0, 1), hamming(0), upto_h(2) => hamming(1), succ_h(3, 5), succ_h(2, 3), succ_h(1, 2), upto_h(2), succ_h(0, 1).
~ succ_h(0, 1), hamming(0), upto_h(3) => hamming(2), succ_h(6, 10), succ_h(5, 6), succ_h(4, 5), succ_h(3, 4), succ_h(2, 3), succ_h(1, 2), upto_h(3), succ_h(0, 1).
~ succ_h(0, 1), hamming(0), upto_h(-1) => upto_h(-1), hamming(0), succ_h(0,1).
