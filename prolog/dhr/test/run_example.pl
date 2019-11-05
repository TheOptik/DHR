:- module(run, [
    op(1200, xfx, =>),
    op(1100, fx, ~)
]).
:- reexport(library(dhr)).
:- use_module(library(dhr/util)).


user:term_expansion(~ Head => Expected, R) :-
    conj_to_list(Head,L),
    findall(H,permutation(L,H),HeadList),
    create_rules(HeadList,Expected,R).

create_rules(HeadList,Expected,Result) :-
    create_rules(HeadList,Expected,[],Result).    

create_rules([],_,Result,Result).
create_rules([H|Hs],Expected,Temp,Result) :-
    conj_to_list(HC,H),
    test_expansion(HC,Expected,R),
    create_rules(Hs,Expected,[R|Temp],Result).


user:term_expansion(Head => Expected, R) :-
    prolog_load_context(module, user),
    test_expansion(Head, Expected, R).


expected_test_result(true, Test, Result) :-
    Test = ( Result = [] ).

expected_test_result(Expected, Test, Result) :-
    xfy_list(',', Expected, Expected_List),
    Test = (
        sort(Result, Re),
        sort(Expected_List, Ex),
        Re = Ex
    ).


:- use_module(library(tap)).

test_expansion(Head, false, Test) :-
    test_expansion(Head, fail, Test).

test_expansion(Head, fail, (TestHead :- TestBody)) :-
    format(atom(TestHead), '~w', [Head]),
    TestBody = \+((
        nb_delete(chr_solution),
        Head,
        nb_getval(chr_solution, _),
        nb_delete(chr_solution)
    )),
    tap:register_test(TestHead).

test_expansion(Head, exception, (TestHead :- TestBody)) :-
    format(atom(TestHead), '~w', [Head]),
    TestBody = catch((
        nb_delete(chr_solution),
        Head,
        nb_getval(chr_solution, _),
        nb_delete(chr_solution),
        fail
    ), Ex, (
        nb_delete(chr_solution),
        writeln(Ex)
    )),
    tap:register_test(TestHead).

test_expansion(Head, Expected, (TestHead :- TestBody)) :-
    format(atom(TestHead), '~w', [Head]),
    expected_test_result(Expected, Test_For_Expected, Result),
    TestBody = (
        nb_delete(chr_solution),
        Head,
        nb_getval(chr_solution, Result),
        nb_delete(chr_solution),
        Test_For_Expected
    ),
    tap:register_test(TestHead).
