:- use_module(library(dhr/test/run_example)).

% Program
:- chr_constraint gcd/1.
gcd(0) <=> true.
gcd(N) \ gcd(M) <=> M >= N | NN is M-N, gcd(NN).

% Tests
~ gcd(6), gcd(9) => gcd(3).
~ gcd(9), gcd(2) => gcd(1).
~ gcd(14), gcd(21) => gcd(7).
~ gcd(32), gcd(48) => gcd(16).
~ gcd(126), gcd(32), gcd(96), gcd(58) => gcd(2).
~ gcd(a), gcd(b) => exception.
