# Execution Context

Prolog module for evaluating JSONPAth directives over Dict structures

## USAGE

## Exported predicates from the module `jsonpath`

### `jsonpath(+jsonpath:atom, +Dict:dict, -Result)` is semidet

Unifies `Result` with the item(s) coresponding to the execution of the `jsonpath` query on the `Dict`. 
See [https://goessner.net/articles/JsonPath/] for the syntaxt of JsonPath. Result is either scalar 
or list of results. Predicate fails if there is no single matching result. 

This predicates is sequence of `jsonpath_compile/2` and `jsonpath_execute/3` calls. In some cases precompiling
the `jsonpath` directive may be beneficial for the execution performance of the system

### `jsonpath_compile(+jsonpath:atom, -Instructions:list(term))` is det

Parses `jsonpath` string and unifies `Instructions` with the list of terms
identifying the requested operation on the dictionary. 

This predicate may be used to precompile `jsonpath` string for more efficient 
execution over multiplicity of dictionaries


### `jsonpath_execute(+Instructions:list(term), +Dict:dict, -Result)` is det

Unifies `Result` with the item(s) coresponding to the execution of 
the JSONPath's `Instructions` query on the `Dict`. `Result` is either scalar 
or list of results. Predicate fails if there is no single matching result. 

This predicate expects precompiled JSONPath `Instructions` produced by `jsonpath_compile/2`
for more efficient execution over multiplicity of dictionaries

## Development

To debug the module, load the `debug.pl` file into prolog top.
