:- use_module(library(dhr/test/run_example)).

% Program
:- chr_constraint add/3.

zero1 @ add(0, Y, Z) <=> Y = Z.
zero2 @ add(X, 0, Z) <=> X = Z.
zero3 @ add(X, Y, 0) <=> X = 0, Y = 0.

same1 @ add(X, E, E) <=> X = 0.
same2 @ add(E, Y, E) <=> Y = 0.

succ1 @ add(s(X), Y, Z) <=> Z = s(W), add(X, Y, W).
succ2 @ add(X, s(Y), Z) <=> Z = s(W), add(X, Y, W).
succ3 @ add(X, X, s(Z)) <=> Z = s(W), X = s(Y), add(Y, Y, W).

search @ add(X, Y, s(Z)) <=> true | add(X1, Y1, Z),
                                ( X = s(X1), Y = Y1 ; X = X1, Y = s(Y1) ).

% Tests
add(s(s(0)),s(s(s(0))),s(s(s(s(s(0)))))) => true.
add(X,s(s(0)),s(s(s(0)))), X = s(0) => true.
add(s(s(0)), s(0), Z), Z = s(s(s(0))) => true.
add(X,Y,s(s(0))), X = s(s(0)), Y = 0 => true. % should this have multiple solutions?
add(X,X,s(s(0))), X = s(0) => true.
add(X,X,s(s(s(0)))) => fail.

~ add(s(0),X,Y), add(X,s(s(0)),s(s(s(0)))), X = s(0), Y = s(s(0)) => true.
