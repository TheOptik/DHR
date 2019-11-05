:- multifile chr_head/1.

load_tests :-
  prolog_load_context(directory, Test_Dir),
  absolute_file_name('examples', Examples_Dir, [
  	relative_to(Test_Dir),
  	file_type(directory)
  ]),
  directory_files(Examples_Dir, Filenames),
  maplist(load_file(Examples_Dir), Filenames).

load_file(_, '.').
load_file(_, '..').
load_file(Examples_Dir, Filename) :-
  absolute_file_name(Filename, Abs_Filename, [relative_to(Examples_Dir)]),
  load_files(Abs_Filename, []).

:- load_tests.
