:- use_module(library(dhr/test/run_example)).

% Program
:- chr_constraint min/1.
min(I) \ min(J) <=> J >= I | true.

% Tests
min(3), min(5), min(5), min(3) => min(3).
min(0) => min(0).
min(1), min(2), min(1) => min(1).
min(1), min(2), min(3), min(4), min(5), min(6), min(7), min(8), min(9) => min(1).
min(-1), min(-2), min(-3), min(-4), min(-5), min(-6), min(-7), min(-8), min(-9) => min(-9).

~ min(-10), min(2), min(0) => min(-10).
~ min(-1), min(-2), min(-0) => min(-2).
~ min(1), min(2), min(0) => min(0).
