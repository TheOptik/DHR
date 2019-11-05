:- use_module(library(dhr/test/run_example)).

% Program
:- chr_constraint fib/2.
f0 @ fib(1, M) <=> M = 1.
f1 @ fib(2, M) <=> M = 1.
fn @ fib(N, M) <=> N > 2 |
          N1 is N-1, N2 is N-2, fib(N1, M1), fib(N2, M2), M is M1+M2.

% Tests
fib(1, R), R = 1 => true.
fib(2, R), R = 1 => true.
fib(5, R), R = 5 => true.
fib(9, R), R = 34 => true.
