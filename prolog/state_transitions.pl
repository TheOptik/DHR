:- module(state_transitions, [chr_solve/2]).
:- use_module(library(dhr_term_expansion)).
:- use_module(library(dhr/util)).
:- use_module(library(debug_util)).

chr_solve(Goal, Result) :-
    goal_to_chr_state(Goal, State),
    (nb_current(chr_solution, Old_Store)->
    (
        State = chr_state(Active, Cont, Store),
        Store = [H|_],
        State2 = chr_state(Active, Cont, [H|Old_Store]), %is this right?
        println(state, State2),
        chr_apply(State2, Result)
    );(
        println(state, State),
        chr_apply(State, Result))
    ).

chr_cont(chr_state(A, (0, C), B), chr_state(A, C, B)):-!.

chr_cont(chr_state(A, Cont, B), chr_state(AN, CN, BN)):-
    println(continuing, Cont),
    reset(Cont, Ball, C),
    println(ball, Ball),
    println(chr_cont, C),
    (C = 0 ->
        (BN = B, AN = A, CN = C);
        (Ball = chr_rule(_, _, _, _, _) ->
            (reset_rule(Ball, chr_state(A, C, B), chr_state(AN, CN, BN)));
            (BN = [Ball|B], AN=Ball, CN = C)
        )
    ).

reset_rule(Rule, S1, S2) :-
    S1 = chr_state(ActiveConstraint, _, ConstraintStore),
    println(filtering, Rule),
    filter_rules([Rule], ConstraintStore, ActiveConstraint, Result, _),
    println(r, Result),
    println(s1, S1),
    (Result = []->
        (S2 = S1);
        (reset_first_rule(Result, S1, S2, [true]))
    ).

chr_apply(chr_state(_, 0, Result), Result):-
    !,
    nb_delete(chr_solution),
    nb_linkval(chr_solution, Result),
    println(set_result, Result).

chr_apply(State, Result) :-
    apply(State, State2),
    chr_apply(State2, Result).


delete_first(_, [], []):-!.
delete_first(Term, [Term|Tail], Tail):- !.
delete_first(Term, [Head|Tail], [Head|Result]) :-
    delete_first(Term, Tail, Result).

remove_list(_, [], []):-!.
remove_list([], L2, L2):-!.
remove_list([L1|L1s], L2, Result) :- delete_first(L1, L2, NL2), remove_list(L1s, NL2, Result).

delete_first_safe(Term, [Term2|Tail], Tail):- Term == Term2, !.
delete_first_safe(Term, [Head|Tail], [Head|Result]) :-
    delete_first_safe(Term, Tail, Result).

%Inputs with Expected Outcomes:
%%The Unifier (Denoted short as U) may be ordered differently, since the result of it's execution should still be the same
%%subset_one_sided(RuleHead, ConstraintStore, Unifier)
%
%subset_one_sided([a(A)], [a(B)], U) -> U=(A=B)
%subset_one_sided([a(A)], [a(B), a(C)], U) -> U=(A=B)
%subset_one_sided([a(A), a(B)], [a(C), a(D)], U) -> U=(A=C, B=D)
%subset_one_sided([a(A), b(B)], [a(C), b(D)], U) -> U=(A=C, B=D)
%subset_one_sided([a(A), b(B)], [b(C), a(D)], U) -> U=(A=D, B=C)
%subset_one_sided([a(A), b(A)], [b(C), a(D)], U) -> fail
%subset_one_sided([a(A), b(A)], [a(B), b(B)], U) -> U=(A=B)
%subset_one_sided([a(A), b(B)], [a(C), b(C)], U) -> U=(A=C, B=C)
%subset_one_sided([a(a(A)), b(b(B))], [a(C), b(C)], U) -> fail
%subset_one_sided([a(A), b(B)], [a(a(C)), b(b(C))], U) -> U=(A=a(C), B=b(C))
%subset_one_sided([a(A), b(A)], [a(a(C)), b(b(C))], U) -> fail
%subset_one_sided([a(A), b(A)], [a(1), b(1)], U) -> U=(A=1)
%subset_one_sided([a(a(A)), b(b(A))], [a(a(1)), b(b(1))], U) -> U=(A=1)
%subset_one_sided([a(b(A), b(B)), a(b(A), b(B))], [a(b(1), b(2)), a(b(1), b(2))], U) -> U=(A=1, B=2)
%
subset_one_sided(RuleHead, ConstraintStore, Unifier, NewConstraintStore) :-
    subset_one_sided(RuleHead, ConstraintStore, [], Unifier, NewConstraintStore).

subset_one_sided([], NewConstraintStore, UnifierList, UnifierList, NewConstraintStore) :- !.
subset_one_sided([E|R], Set, UnifierAcc, Unifier, NewConstraintStore) :-
    member_one_sided(E, Set, SetEntry, NewUnifier),
    delete_first_safe(SetEntry, Set, Set2),
    (combine_unifiers(NewUnifier, UnifierAcc, CombinedUnifier) ->
        subset_one_sided(R, Set2, CombinedUnifier, Unifier, NewConstraintStore);
        fail%subset_one_sided(R, Set2, UnifierAcc, Unifier)
    ). 

create_callable_unifier([], []).
create_callable_unifier([L|Ls], [C|Cs]) :- L=(A-B), C = (A=B), create_callable_unifier(Ls, Cs).

member_unifier_safe((H-_), [Result|_], Result) :- Result = (L-_), H == L.
member_unifier_safe(Element, [(L-_)|Ls], Result) :- Element = (H-_), \+(H == L), member_unifier_safe(Element, Ls, Result).

combine_unifiers(NewUnifier, UnifierAcc, CombinedUnifier) :-
    NewUnifier = (Head-Store),
    var(Head),
    member_unifier_safe((Head-_), UnifierAcc, Result),
    Result = (_-R), Store == R,
    CombinedUnifier = UnifierAcc.

combine_unifiers(NewUnifier, UnifierAcc, CombinedUnifier) :-
    NewUnifier = (Head-_),
    var(Head),
    \+(member_unifier_safe((Head-_), UnifierAcc, _)),
    %NewUnifier2 = (Store-Head),
    %CombinedUnifier = [NewUnifier,NewUnifier2|UnifierAcc].
    CombinedUnifier = [NewUnifier|UnifierAcc].

combine_unifiers(NewUnifier, UnifierAcc, CombinedUnifier) :-
    NewUnifier = (Head-Store),
    nonvar(Head),
    Head =.. [_|NewHeads],
    Store =.. [_|NewStores],
    combine_unifiers(NewHeads, NewStores, UnifierAcc, CombinedUnifier).

combine_unifiers([], [], CombinedUnifier, CombinedUnifier).
combine_unifiers([H|Hs], [S|Ss], UnifierAcc, CombinedUnifier):-
    combine_unifiers((H-S), UnifierAcc, UnifierAcc2),
    combine_unifiers(Hs, Ss, UnifierAcc2, CombinedUnifier).


member_one_sided(Element, [X|_], MatchingSetEntry, Unifier) :-
    subsumes_term(Element, X),
    MatchingSetEntry = X,
    Unifier = (Element - X),
    !.

member_one_sided(Element, [_|Xs], MatchingSetEntry, Unifier) :-
    %Element can not be one-sidedly unified with first member.
    member_one_sided(Element, Xs, MatchingSetEntry, Unifier).

simplify_continuation(Cont, Result) :-
    simplify_continuation(Cont, true, Result).

simplify_continuation(0, Result, Result).
simplify_continuation(Cont, Temp, Result) :-
    \+(Cont = (_, _)),
    Result = (Temp, Cont).
simplify_continuation(Cont, Temp, Result) :-
    %Cont has to be conjunction
    Cont = (0, B),
    simplify_continuation(B, Temp, Result).

simplify_continuation(Cont, Temp, Result) :-
    %Cont has to be conjunction
    % A is not 0
    Cont = (A, B),
    Temp = true,
    simplify_continuation(B, A, Result).

simplify_continuation(Cont, Temp, Result) :-
    %Cont has to be conjunction
    % A is not 0 and Temp is not true
    Cont = (A, B), 
    simplify_continuation(B, (Temp, A), Result).

filter_rules(Rules, Constraint_Store, ActiveConstraint, Result, Unifiers) :-
    filter_rules(Rules, Constraint_Store, ActiveConstraint, [], Result, [], Unifiers).

filter_rules([], _, _, Result, Result, Unifiers, Unifiers).

filter_rules([R|Rs], Constraint_Store, ActiveConstraint, Temp, Result, TempUnifier, Unifier) :-
    chr_rule(Active, Head_Keep, Head_Remove, _, Guard) = R,
    permutation(Perm, Constraint_Store),
    subset_one_sided(Head_Remove, Perm, UnifierList1, New_Constraint_Store),
    subset_one_sided(Head_Keep, New_Constraint_Store, UnifierList1, UnifierList2, _),
    create_callable_unifier(UnifierList2, CallableUnifierList), conj_to_list(CombinedUnifier, CallableUnifierList),
    copy_term(((CombinedUnifier, Guard)-(ActiveConstraint, Active)), Copy),
    Copy = (GuardUnifierCopy-ActiveCopy),
    call(GuardUnifierCopy),
    ActiveCopy = (A, AC), A == AC,
    println(rule,R),
    println(store,Constraint_Store),
    println(unifier1,UnifierList1),
    println(unifier2,UnifierList2),
    !,
    filter_rules(Rs, Constraint_Store, ActiveConstraint, [R|Temp], Result, [CombinedUnifier|TempUnifier], Unifier).

filter_rules([_|Rs], Constraint_Store, ActiveConstraint, Temp, Result, TempUnifier, Unifier) :-
    %R is not applicable with current constraint store
    filter_rules(Rs, Constraint_Store, ActiveConstraint, Temp, Result, TempUnifier, Unifier).


find_all_applicable_rules(ActiveConstraint, Constraint_Store, Result, Unifiers) :-
    findall(chr_rule(ActiveConstraint, Head_Keep, Head_Remove, Body, Guard), %does this unify already?
    chr_rule(ActiveConstraint, Head_Keep, Head_Remove, Body, Guard),
    R),
    println(found, R),
    filter_rules(R, Constraint_Store, ActiveConstraint, ResultReversed, ReversedUnifiers),
    println(filtered, ResultReversed),
    reverse(ResultReversed, Result),
    reverse(ReversedUnifiers, Unifiers).

wrap_rules_in_shift(Rules, Wrapped_Rules) :-
    wrap_rules_in_shift(Rules, [], Wrapped_Rules).

wrap_rules_in_shift([], ResultR, Result) :-
    reverse(ResultR, Result).

wrap_rules_in_shift([R|Rs], Temp, Result) :-
    R = chr_rule(_, _, _, _, _),
    wrap_rules_in_shift(Rs, [shift(R)|Temp], Result).

wrap_rules_in_shift([R|Rs], Temp, Result) :-
    %R is not a chr_rule
    wrap_rules_in_shift(Rs, [R|Temp], Result).

combine_rules_and_unifiers(Rules, Unifiers, Result):-
    combine_rules_and_unifiers(Rules, Unifiers, [], Result).

combine_rules_and_unifiers([], [], Result, Result).

combine_rules_and_unifiers([R|Rs], [U|Us], ResultAcc, Result):-
    R = chr_rule(Active, HeadKeep, HeadRemove, Body, Guard),
    combine_rules_and_unifiers(Rs, Us, [chr_rule(Active, HeadKeep, HeadRemove, (U, Body), Guard)|ResultAcc], Result).



apply(chr_state(Active, Continuations, Constraint_Store),
    chr_state(ActiveN, ContinuationN, Constraint_StoreN)) :-
        println(active, Active),
        find_all_applicable_rules(Active, Constraint_Store, Applicable_Rules, Unifiers),
        %combine_rules_and_unifiers(Applicable_Rules, Unifiers, CombinedRules),
        println(applicable, Applicable_Rules),
        reset_first_rule(Applicable_Rules, Unifiers, chr_state(Active, Continuations, Constraint_Store), chr_state(ActiveN, ContinuationN, Constraint_StoreN)).

reset_first_rule([], _, chr_state(Active, Continuations, Constraint_Store), chr_state(ActiveN, ContinuationN, Constraint_StoreN)) :-
    State = chr_state(Active, Continuations, Constraint_Store),
    println(calling_cont, State),
    chr_cont(State, State2),
    println(after, State2),
    chr_state(ActiveN, ContinuationN, Constraint_StoreN) = State2,
    !.

reset_first_rule([R|Rs], [U|_], chr_state(Active, Continuations, Constraint_Store), chr_state(ActiveN, ContinuationN, Constraint_StoreN)) :-
    println(active,Active),
    R = chr_rule(_, _, Head_Remove, Body, _),
    println(resetting, Body),
    call(U),
    reset(Body, Ball, Continuation),
    println(cont,Continuation),
    println(ball, Ball),
    ((Ball == true;var(Ball)) ->
        (Constraint_StoreR = Constraint_Store);
        (Constraint_StoreR = [Ball|Constraint_Store])
    ),
    remove_list(Head_Remove, Constraint_StoreR, Constraint_StoreN),
    selectNewActiveConstraint(Active,Ball,Constraint_StoreN,ActiveN),
    (Rs = [] ->(simplify_continuation((Continuation,Continuations), ContinuationN));(
    println(unwrapped,Rs),
    wrap_rules_in_shift(Rs,R_wrapped),
    println(wrapped,R_wrapped),
    conj_to_list(C,R_wrapped),
    simplify_continuation((Continuation,(C, Continuations)), ContinuationN))),
    println(after, chr_state(ActiveN, ContinuationN, Constraint_StoreN)),
    println(simplified_continuation, ContinuationN),
    !.



selectNewActiveConstraint(Active, _, [], ActiveN) :-   
    ActiveN = Active.

selectNewActiveConstraint(_, Ball, ConstraintStore, ActiveN) :-
    var(Ball),
    [ActiveN|_]=ConstraintStore.

selectNewActiveConstraint(_, true, _, ActiveN) :-
    ActiveN=true.

selectNewActiveConstraint(_, _, ConstraintStore, ActiveN) :-   
    [ActiveN|_]=ConstraintStore.