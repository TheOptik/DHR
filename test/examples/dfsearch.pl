:- use_module(library(dhr/test/run_example)).

% Program
:- chr_constraint dfsearch/2.
leaf          @ dfsearch(leaf(Val), X) <=> X == Val.
node_found    @ dfsearch(node(Val, _, _), Val) <=> true.
node_search   @ dfsearch(node(Val, L, R), X) <=> X \== Val |
                  ( dfsearch(L, X) ; dfsearch(R, X) ).

% Tests
dfsearch(node(0, node(1, leaf(2), leaf(3)), leaf(4)), 0) => true.
dfsearch(node(0, node(1, leaf(2), leaf(3)), leaf(4)), 4) => true.
dfsearch(node(0, node(1, leaf(2), leaf(3)), leaf(4)), 5) => false.
