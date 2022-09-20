## MVP Language Syntax BNF Form and Semantics

### Program

    <program'>  ::= <program> <EOF>
    <program>   ::= <func>*

### Function

    <func>          ::= `fun` <ident> `(` <param-list>? `)` `:` <type> `do`
                            <stat>
                        `end`
    <param-list>    ::= <ident> `:` <type> (`,` <ident> `:` <type>)*

- Functions must return a value (i.e. void functions cannot exist).
- All possible pathways for a function must end with either `exit` or `return`.
  - Otherwise a syntax error.
- At least one `main` function must exist, which will be the entry point of the program.
  - Otherwise a semantic error.

### Statement

    <stat>          ::= <stat-one>+
    <stat-one>      ::= `skip` `;`
                    |   `var` <ident> `:` <type> `=` <expr> `;`
                    |   <ident> `=` <expr> `;`
                    |   `scan` <ident> `;`
                    |   (`print` | `exit` | `return`) <expr> `;`
                    |   `if` <expr> `then` <stat> `else` <stat> `fi`
                    |   `while` <expr> `do` <stat> `done`
                    |   `do` <stat> `done`

### Type

    <type>      ::= `int`
                |   `bool`
                |   `char`
                |   `string`

- There are no explicit (or implicit) typecasting.
- `int` is a 32-bit integer.
- `int` type is represented in two's complement form.
- `char` types only support ASCII characters.

### Expression

    <expr>          ::= <expr'> (<binary-op> <expr'>)*
    <expr'>         ::= <int-liter> | <bool-liter> | <char-liter> | <str-liter>
                    |   <ident> <arg-list>?
                    |   <unary-op> <expr>
                    |   `(` <expr> `)`
    <arg-list>      ::= `(` (<expr> (`,` <expr>)*)? `)`
    <unary-op>      ::= `-` | `not` | `len` | `ord` | `chr`
    <binary-op>     ::= `*` | `/` | `+` | `-` | `mod` | `>` | `>=` | `<` | `<=`
                    |   `==` | `!=` | `and` | `or`

| Operator | Precedence |             Arg 1            |             Arg 2            |   Ret  |
|:--------:|:----------:|:----------------------------:|:----------------------------:|:------:|
|    `*`   |      1     |             `int`            |             `int`            |  `int` |
|    `/`   |      1     |             `int`            |             `int`            |  `int` |
|   `mod`  |      1     |             `int`            |             `int`            |  `int` |
|    `+`   |      2     |             `int`            |             `int`            |  `int` |
|    `-`   |      2     |             `int`            |             `int`            |  `int` |
|    `>`   |      3     |         `int`/`char`         |         `int`/`char`         | `bool` |
|   `>=`   |      3     |         `int`/`char`         |         `int`/`char`         | `bool` |
|    `<`   |      3     |         `int`/`char`         |         `int`/`char`         | `bool` |
|   `<=`   |      3     |         `int`/`char`         |         `int`/`char`         | `bool` |
|   `==`   |      4     | `int`/`char`/`bool`/`string` | `int`/`char`/`bool`/`string` | `bool` |
|   `!=`   |      4     | `int`/`char`/`bool`/`string` | `int`/`char`/`bool`/`string` | `bool` |
|   `and`  |      5     |            `bool`            |            `bool`            | `bool` |
|   `or`   |      6     |            `bool`            |            `bool`            | `bool` |

### Lexeme

    <ident>         ::= (`_` | a-zA-Z) (`_` | a-zA-Z0-9)*
    <int-liter>     ::= (`+` | `-`)? (0-9)+
    <bool-liter>    ::= `true` | `false`
    <char-liter>    ::= `'` <char> `'`
    <str-liter>     ::= `"` <char>* `"`
    <char>          ::= any-ASCII-except-`\`-`'`-`"` | `\` <esc-char>
    <esc-char>      ::= `0` | `t` | `n` | `r` | `"` | `'` | `\`
    <comment>       ::= `#` any-char-except-EOL* <EOL>
