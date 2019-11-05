:- module(dhr_term_expansion, [

    %Operator Definitions
    op(1180, xfx, ==>),
    op(1180, xfx, <=>),
    op(1150, fx, chr_constraint),
    op(1100, xfx, \),
    op(1200, xfx, @),

    %Exported Rules
    goal_to_chr_state/2,
    user:chr_rule/5
]).
:- use_module(library(dhr/util)).
:- use_module(library(debug_util)).

:- discontiguous user:chr_head/1. %is this necessary?

user:expand_query(end_of_file, _, _, _) :-
    !,
    fail.

user:expand_query(Query, (Query -> (Post); (nb_delete(chr_solution), fail)), Bindings, Bindings) :- % cleanup no matter if the query fails or not
    conj_to_list(Query, Q_L),
    expand_chr_query(Q_L, Post).

expand_chr_query([], true).
expand_chr_query([Q|Qs], Post) :-
    user:chr_head(Q)->
    Post = (
        nb_getval(chr_solution, Result),
        writeln(Result),
        nb_delete(chr_solution)
        );
    expand_chr_query(Qs, Post).


%CHR
user:term_expansion( (:- chr_constraint X), Rules) :-
    chr_constraint_expander(X, [], Rules).

chr_constraint_expander([], R, R).
chr_constraint_expander([Name/Arity|Xs], Rules, RulesResult) :-
    functor(Head, Name, Arity),
    !,
    chr_constraint_expander(Xs, [(Head :- chr_solve(Head, _), !),user:chr_head(Head)|Rules], RulesResult).

chr_constraint_expander(X, R1, R2) :-
    conj_to_list(X, X_List),
    chr_constraint_expander(X_List, R1, R2).

%chr_rule(Active, HeadKeep, HeadRemove, Body, Guard).
:- dynamic user:chr_rule/5.
:- multifile user:chr_rule/5.


%chr_state(ActiveConstraint, Continuations, ConstraintStore)
goal_to_chr_state(G, S) :-
    conj_to_list(G, G_List),
    [Active|_] = G_List,
    println(initActive, Active),
    S = chr_state(Active, call_continuation([]), G_List).



%Term Expansions
user:term_expansion(_@B, R):- user:term_expansion(B, R).

user:term_expansion(Head1\Head2 <=> Body_W_Guard, Rules) :-
    conj_to_list(Head1, Head1_List),
    conj_to_list(Head2, Head2_List),
    create_chr_rule(Head1_List, Head2_List, Body_W_Guard, Rules).

user:term_expansion(Head ==> Body_W_Guard, Rules) :-
    conj_to_list(Head, Head1_List),
    create_chr_rule(Head1_List, [], Body_W_Guard, Rules).


user:term_expansion(Head <=> Body_W_Guard, Rules) :-
    conj_to_list(Head, Head2_List),
    create_chr_rule([], Head2_List, Body_W_Guard, Rules).

create_chr_rule(Head1_List, Head2_List, Body_W_Guard, Rules) :-
    transform_guard_to_conjunction(Body_W_Guard, Guard_Body),
    (Guard, Body) = Guard_Body,
    wrap_constraints_with_shift(Body, Body_W_Shift),
    append(Head1_List,Head2_List,Heads),
    createRules(Heads, Head1_List, Head2_List, Body_W_Shift, Guard, [], Rules).

createRules([], _, _, _, _, Rules, Rules).

createRules([H|Hs], Head1_List, Head2_List, Body_W_Shift, Guard, Temp, Rules) :-
    T = user:chr_rule(H, Head1_List, Head2_List, Body_W_Shift, Guard),
    createRules(Hs, Head1_List, Head2_List, Body_W_Shift, Guard, [T|Temp], Rules).



transform_guard_to_conjunction((Guard | Body), (Guard, Body)).

transform_guard_to_conjunction(Body, (true, Body)).

wrap_constraints_with_shift(true,shift(true)).

wrap_constraints_with_shift(Body, Result):-
    conj_to_list(Body, Body_List),
    reverse(Body_List, R_Body_List),
    wrap_constraints_with_shift(R_Body_List, [], R),
    conj_to_list(Result, R).

wrap_constraints_with_shift([], Body, Body).

wrap_constraints_with_shift([B|Bs], Temp, Body):-
    user:chr_head(B) ->
    wrap_constraints_with_shift(Bs, [shift(B)|Temp], Body);
    wrap_constraints_with_shift(Bs, [B|Temp], Body).
