:- use_module(library(dhr/test/run_example)).

% Program
:- chr_constraint a/1, b/1.
a(0) <=> b(0).
a(X) <=> X > 0 | X1 is X-1, b(X), a(X1).

% Tests
a(0) => b(0).
a(1) => b(0), b(1).
a(2) => b(0), b(1), b(2).
a(4) => b(0), b(1), b(2), b(3), b(4).
a(9) => b(0), b(1), b(2), b(3), b(4), b(5), b(6), b(7), b(8), b(9).
