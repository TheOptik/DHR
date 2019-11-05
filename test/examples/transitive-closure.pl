:- use_module(library(dhr/test/run_example)).

% Program
:- chr_constraint edge/2, path/2.
rem_dup  @ path(X, Y) \ path(X, Y) <=> true.
path_1   @ edge(X, Y) ==> path(X, Y).
path_add @ edge(X, Y), path(Y, Z) ==> X \== Y, Y \== Z | path(X, Z).

% Tests
edge(1, 2), edge(2, 3), edge(2, 4) => edge(2, 4), edge(2, 3), edge(1, 2), path(1, 4),
                                    path(2, 4), path(1, 3), path(2, 3), path(1, 2).
