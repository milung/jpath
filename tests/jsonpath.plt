:- begin_tests(jsonpath).
:- use_module(prolog/jsonpath).

test('jsonpath', 
    [
        nondet,
        forall( member(Path = Result, [
            '$..author' = ["Nigel Rees","Evelyn Waugh", "Herman Melville","J. R. R. Tolkien"],
            '$.store..author' = ["Nigel Rees","Evelyn Waugh", "Herman Melville","J. R. R. Tolkien"],
            '$.store.book[0].author' = "Nigel Rees",
            '$.store.book[1].author' = "Evelyn Waugh",
            '$.store.book[?(@.title="Sayings of the Century")].author' = "Nigel Rees",
            '$.store.book[*].author' = ["Nigel Rees","Evelyn Waugh", "Herman Melville","J. R. R. Tolkien"],
            '$.store.book[1:2].author' = "Evelyn Waugh",
            '$.store.book[3:].author' = "J. R. R. Tolkien",
            '$.store.book[:2].author' = ["Nigel Rees","Evelyn Waugh"],
            '$.store.book[0:3:2].author' = ["Nigel Rees", "Herman Melville"],
            '$.store.book[@.length-2].author' = "Herman Melville",
            '$.store.book[last()].author' = "J. R. R. Tolkien",
            '$.store.book[position()>1].author' = ["Herman Melville","J. R. R. Tolkien"]
            ]))
    ]) 
:-
    % prepare
    store_json(Dict),
    % execute
    jsonpath:jsonpath(Path, Dict, Result).


test('jsonpath_compile', 
    [
        nondet,
        forall( 
          member(Path = Result, [
            '$.store.book[0].author' = [
                get_root, 
                get_property(store),
                get_property(book), 
                get_index(0), 
                get_property(author)
            ],
            '$.store.book[?(@.title="Sayings of the Century")].author' = [
                get_root, 
                get_property(store),
                get_property(book), 
                filter([get_current, get_property(title)], equal,  'Sayings of the Century'),
                get_property(author)
            ],
            '$.store.book[*].author' = [
                get_root, 
                get_property(store),
                get_property(book), 
                get_slice(0, end, 1),
                get_property(author)
            ],
            '$..author' = [
              get_root,
              expand,
              get_property(author)
            ]
          ])
        )
    ])
:-
    % execute
    jsonpath:jsonpath_compile(Path, Instructions), 
    Instructions = Result.

:- end_tests(jsonpath).

store_json(_{ store:_{
  book: [ 
    _{ category: "reference",
      author: "Nigel Rees",
      title: "Sayings of the Century",
      price: 8.95
    },
    _{ category: "fiction",
      author: "Evelyn Waugh",
      title: "Sword of Honour",
      price: 12.99
    },
    _{ category: "fiction",
      author: "Herman Melville",
      title: "Moby Dick",
      isbn: "0-553-21311-3",
      price: 8.99
    },
    _{ category: "fiction",
      author: "J. R. R. Tolkien",
      title: "The Lord of the Rings",
      isbn: "0-395-19395-8",
      price: 22.99
    }
  ],
  bicycle: _{
    color: "red",
    price: 19.95
  }
}
}).