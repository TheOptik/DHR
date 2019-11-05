:- use_module(library(dhr/test/run_example)).

% Program
:- chr_constraint tak/4.
tak(X,Y,Z,A) <=> X =< Y | Z = A.
tak(X,Y,Z,A) <=> X > Y | 
        X1 is X-1, Y1 is Y-1, Z1 is Z-1,
        tak(X1,Y,Z,A1), tak(Y1,Z,X,A2), tak(Z1,X,Y,A3),
        tak(A1,A2,A3,A).

% Tests
tak(1, 2, 3, R), R = 3 => true.
tak(2, 2, 2, R), R = 2 => true.
tak(20, 5, 1, R), R = 5 => true.
tak(8, 4, 2, R), R = 3 => true.
tak(12, 6, 3, R), R = 4 => true.
tak(18, 12, 6, R), R = 7 => true.
tak(18, 12, _, 7) => exception.
tak(18, _, 6, 7) => exception.
tak(_, 12, 6, 7) => exception.
tak(_, _, _, _) =>  exception.
