:- module(debug_util, [
    println/2    
    ]).


println(Tag, Message) :-
    current_prolog_flag(debug, true)->
    (
    write('['),
    print(Tag),
    write(']: '),
    print(Message),
    nl
    );true.
