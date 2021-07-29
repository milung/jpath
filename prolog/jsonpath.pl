:- module(jsonpath, [
    jsonpath/3,            %  +Path:atom, +Dict:dict, -Result
    jsonpath_compile/2,    % +jsonpath:atom, -Instructions:list(term)
    jsonpath_execute/3     % +Instructions:list(term), +Dict:dict, -Result
    ]).
%! <module> jsonpath doing some cool things
%  Predicates for jsonpath ...

:- use_module(library(dcg/basics)).

% :- use_module(source(module)).

%%% PUBLIC PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%%

%! jsonpath(+jsonpath:atom, +Dict:dict, -Result) is semidet
%  Unifies `Result` with the item(s) coresponding to the execution of the `jsonpath` query on the `Dict`. 
%  See [https://goessner.net/articles/JsonPath/] for the syntaxt of JsonPath. Result is either scalar 
%  or list of results. Predicate fails if there is no single matching result. 
%  
%  This predicates is sequence of `jsonpath_compile/2` and `jsonpath_execute/3` calls. In some cases precompiling
%  the `jsonpath` directive may be beneficial for the execution performance of the system
jsonpath(Path, Dict, Result) :-
    jsonpath_compile(Path, Instructions),
    !,
    jsonpath_execute(Instructions, Dict, Results),
    (   length(Results, 0)
    ->  fail
    ;   length(Results, 1)
    ->  Results = [ Result ]
    ;   Result = Results
    ),
    !.

%! jsonpath_compile(+jsonpath:atom, -Instructions:list(term)) is det
%  Parses `jsonpath` string and unifies `Instructions` with the list of terms
%  identifying the requested operation on the dictionary. 
% 
%  This predicate may be used to precompile `jsonpath` string for more efficient 
%  execution over multiplicity of dictionaries
jsonpath_compile(Path, Instructions) :-
    atom_codes(Path, PathCodes),
    phrase(jsonpath_program(Instructions), PathCodes).

%! jsonpath_execute(+Instructions:list(term), +Dict:dict, -Result) is det
%  Unifies `Result` with the item(s) coresponding to the execution of 
%  the JSONPath's `Instructions` query on the `Dict`. `Result` is either scalar 
%  or list of results. Predicate fails if there is no single matching result. 
% 
%  This predicate expects precompiled JSONPath `Instructions` produced by `jsonpath_compile/2`
%  for more efficient execution over multiplicity of dictionaries
jsonpath_execute(Instructions, Dict, Result) :-
   (var(Instructions) -> instantiation_error(Instructions); true ),
   foldl(instruction(Dict), Instructions, [Dict], Result).
%%% PRIVATE PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%

expand_dictionary(Dict, Expanded) :-
   phrase(expand_dictionaries([Dict]), Expanded).

expand_dictionary(Dict) -->
   [Dict],
   {  findall(Value, get_dict(_, Dict, Value), Expansion)
   },
   expand_dictionaries(Expansion).

expand_dictionaries([]) --> [], !.
 expand_dictionaries([H|Dicts]) -->
   {  is_list(H) },
   !,
   expand_dictionaries(H),
   expand_dictionaries(Dicts).
 expand_dictionaries([H|Dicts]) -->
   { is_dict(H) },
   !,
   expand_dictionary(H),
   expand_dictionaries(Dicts).
 expand_dictionaries([_|Dicts]) -->
   expand_dictionaries(Dicts).
   
get_position(_, _, _, [], []).
 get_position(Operator, Operand, Index, [H|List], Out) :-
   (  expr_operation(Operator, Operand, Index)
   -> Out = [H|Out0]
   ;  Out = Out0
   ),
   succ(Index, Index1),
   get_position(Operator, Operand, Index1, List, Out0).

get_slice(Start, end, Step, In, Out) :-
   length( In, End),
   get_slice(Start, End, Step, In, Out), 
   !.
 get_slice(Start, End, Step, In, Out) :-
   Start < 0,
   length( In, Length),
   Start1 is Length - Start,
   get_slice(Start1, End, Step, In, Out), 
   !.  
 get_slice(Start, End, Step, In, Out) :-
   End < 0,
   length( In, Length),
   End1 is Length - End,
   get_slice(Start, End1, Step, In, Out), 
   !.  
 get_slice(Start, End, Step, In, Out) :-  
   Step < 0,
   length( In, Length),
   reverse(In, Rev),
   StartRev is Length - Start -1,
   EndRev is Length - End,
   StepRev is Step * -1,
   get_slice(StartRev, EndRev, StepRev, Rev, OutRev),
   reverse(OutRev, Out),
   !.
 get_slice(Start, End, _, In, []) :-
   (  Start >= End
   ;  length( In, Length),
      Start >= Length
   ),
   !.
 get_slice(Start, End, Step, In, Out) :-
   must_be(positive_integer, Step),
   must_be(nonneg, Start),
   must_be(nonneg, End),
   must_be(list, In),   
   get_slice_impl(Start, End, Step, In, Out),
   !.

get_slice_impl(_, _, _, [], []) :- !.
 get_slice_impl(_, End, _, _, []) :-
   End =< 0,
   !.
 get_slice_impl(Start, End, Step, [H|In], Out) :-
   End1 is End - 1,   
   (  Start > 0
   -> Start1 is Start -1, 
      Out0 = Out
   ;  Start1 is Step -1,
      Out = [ H | Out0 ]
   ),   
   get_slice_impl(Start1, End1, Step, In, Out0).

filter(Instructions, Operation, Operand, Array, Filtered) :-
   include( {Instructions, Operation, Operand}/[C] >> (
                  jsonpath_execute(Instructions, C, [Result]),
                  expr_operation(Operation, Result, Operand)
            ), 
            Array, 
            Filtered). 

instruction(Root, get_root, _, [Root]).
 instruction(_, get_current, Current, Current).
 instruction(_, get_slice(Start, End, Step), Currents, Result) :-
    convlist(
        get_slice(Start, End, Step),
        Currents, 
        Slices 
    ),
    append(Slices, Result).
 instruction(_, get_index(Index), Currents, WithIndex) :-
    convlist(
        {Index}/[C,I] >> (
            is_list(C),
            (  Index >= 0
            -> nth0(Index, C, I)
            ;  length(C, Length),
               is(Index1, +(Length,Index)),
               nth0(Index1, C, I)
            )
        ),
        Currents, 
        WithIndex 
    ).
 instruction(_, get_property(Property), Currents, WithIndex) :-
    convlist(
        {Property}/[C,I] >> (
            is_dict(C),
            get_dict(Property, C, I)
        ),
        Currents, 
        WithIndex 
    ).
 instruction(_, get_position(Operator, Operand), Currents, Results) :-
    convlist( get_position(Operator, Operand, 0), Currents, Results0 ),
    append(Results0, Results).
 instruction(_, filter(Instructions, Operation, Operand), Currents, Results) :-
   convlist( filter(Instructions, Operation, Operand), Currents, Results0 ),
   append(Results0, Results).
 instruction(_, expand, Currents, Results) :-
   convlist( expand_dictionary, Currents, Results0 ),
   append(Results0, Results).

expr_operand(true), ")" --> ")".
 expr_operand(Value) -->
    "'", string_without("'", Codes), "'",
    {   atom_codes(Value, Codes)},
    !.
 expr_operand(Value) -->
    "\"", string_without("\"", Codes), "\"",
    {   atom_codes(Value, Codes)},
    !.
 expr_operand(Value) -->
    number(Value),
    !.
 expr_operand(Value) -->
    string_without(")", Codes), 
    {   atom_codes(Value, Codes) }.

expr_operation(Op, Left, Right) :-
    string(Left),
    atom_string(LeftA, Left),
    expr_operation(Op, LeftA, Right).
 expr_operation(Op, Left, Right) :-
    string(Right),
    atom_string(RightA, Right),
    expr_operation(Op, Left, RightA).
 expr_operation(equal, Operand, Operand).
 expr_operation(not_equal, Operand, Input) :-
    Input \= Operand.
 expr_operation(less, Operand, Input) :-
    Input < Operand.
 expr_operation(less_equal, Operand, Input) :-
    Input =< Operand.
 expr_operation(greater, Operand, Input) :-
    Input > Operand.
 expr_operation(greater_equal, Operand, Input) :-
    Input >= Operand. 
    

expr_operator(less) --> "<".
 expr_operator(less_equal) --> "<=".
 expr_operator(greater) --> ">".
 expr_operator(greater_equal) --> ">=".
 expr_operator(equal) --> "=".
 expr_operator(equal) --> "==".
 expr_operator(equal), ")" --> ")".
 expr_operator(not_equal) --> "<>".
 expr_operator(not_equal) --> "!=".

jsonpath_instruction(get_root) --> [ 0'$ ].
 jsonpath_instruction(get_current) --> "@".
 jsonpath_instruction(expand), "." --> "..".
    
 jsonpath_instruction(get_property(Property)) --> 
    ".", 
    property_name(Property).
 jsonpath_instruction(get_slice(0, end, 1)) --> "[*]".
 jsonpath_instruction(get_index(Index1)) --> 
    "[@.length", "-", integer(Index), "]",
    { Index1 is -Index }.
 jsonpath_instruction(get_index(-1)) --> "[last()]".
 jsonpath_instruction(get_position(Operator, Operand )) --> 
    "[position()",
    expr_operator(Operator),
    expr_operand(Operand),
    "]".
 jsonpath_instruction(filter(SubInstructions, Operator, Operand )) --> 
    "[?(",
    jsonpath_program(SubInstructions),
    expr_operator(Operator),
    expr_operand(Operand),
    ")]".
 jsonpath_instruction(get_property(Property)) --> 
    "[", property_name(Property), "]".
 jsonpath_instruction(get_index(Index)) --> 
    ".", integer(Index).
 jsonpath_instruction(get_slice(Start, End, Step)) --> 
    "[", integer(Start), ":", integer(End), ":", integer(Step), "]".
 jsonpath_instruction(get_slice(Start, End, 1)) --> 
    "[", integer(Start), ":", integer(End), "]".
 jsonpath_instruction(get_slice(Start, end, 1)) --> 
    "[", integer(Start), ":]".
 jsonpath_instruction(get_slice(0, End, 1)) --> 
    "[:", integer(End), "]".
 jsonpath_instruction(get_slice(0, End, Step)) --> 
    "[:", integer(End), ":", integer(Step), "]".
 jsonpath_instruction(get_slice(0, end, Step)) --> 
    "[::", integer(Step), "]".
 jsonpath_instruction(get_index(Index)) --> 
    "[", integer(Index), "]".
 

jsonpath_program([I|Instructions]) -->
    jsonpath_instruction(I), 
    jsonpath_program(Instructions).
 jsonpath_program([]) --> [].

property_name(Name) --> 
    [First], 
    {   is_alpha(First)
    ;   First == 0'_
    ;   First == 0'$
    },
    property_name_codes(Codes),
    {
        atom_codes(Name, [First|Codes])
    },
    !.
 property_name(Name) --> 
    [0''], 
    string_without("'", Field),
    [0''], 
    {   atom_codes(Name, Field)},
    !.
 property_name(Name) --> 
    [0'"], 
    string_without("'", Field),
    [0'"], 
    {   atom_codes(Name, Field)},
    !.

property_name_codes([C|Codes]) -->
    [C],
    {   is_alpha(C)
    ;   is_digit(C)
    ;   C == 0'_
    ;   C == 0'$
    },
    property_name_codes(Codes).
 property_name_codes([]) --> [].


 