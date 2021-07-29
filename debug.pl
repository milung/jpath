%% start your debugging session by loading 'debug' file. 


:- set_test_options([run(make)]).

:- current_prolog_flag(gui, true)
-> guitracer
; true.

:- use_module(prolog/jsonpath).
:- ['tests/jsonpath.plt'].