:- module(dhr_util, [
    conj_to_list/2,
    xfy_list/3,
    subsumes/3
]).

conj_to_list(true,[]).
conj_to_list((H, T), [H|Conj]) :-
    conj_to_list(T, Conj),
    !.
conj_to_list(H, [H]).


% Identical to list_util:xfy_list/3. Copied here so that library(dhr)
% can have no pack dependencies. That lets other packs use library(dhr)
% without circular dependencies.
xfy_list(Op, Term, [Left|List]) :-
    Term =.. [Op, Left, Right],
    xfy_list(Op, Right, List),
    !.
xfy_list(_, Term, [Term]).

subsumes(Term1,Term2,Unifier) :-
	empty_assoc(S0),
	subsumes_aux(Term1,Term2,S0,S),
	assoc_to_list(S,L),
	build_unifier(L,Unifier).

subsumes_aux(Term1, Term2, S0, S) :-
        (   compound(Term2),
            functor(Term2, F, N)
        ->  compound(Term1), functor(Term1, F, N),
            subsumes_aux(N, Term1, Term2, S0, S)
        ;   Term1 == Term2
	->  S = S0
	;   var(Term2),
	    get_assoc(Term1,S0,V)
	->  V == Term2, S = S0
	;   var(Term2),
	    put_assoc(Term1, S0, Term2, S)
        ).

subsumes_aux(0, _, _, S, S) :- ! .
subsumes_aux(N, T1, T2, S0, S) :-
        arg(N, T1, T1x),
        arg(N, T2, T2x),
        subsumes_aux(T1x, T2x, S0, S1),
        M is N-1,
        subsumes_aux(M, T1, T2, S1, S).

build_unifier([],[]).
build_unifier([X-V|R],[V - X | T]) :-
	build_unifier(R,T).
