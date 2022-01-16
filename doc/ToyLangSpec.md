## Language Syntax in BNF form and Language Semantics

### Program

    <program'>  ::= <program> <EOF>
    <program>   ::= <func>*

### Function

    <func>          ::= `fun` <ident> `(` <param-list>? `)` `:` (<type> | `void`) `do`
                            <stat>
                        `end`
    <param-list>    ::= <ident> `:` <type> (`,` <ident> `:` <type>)*

- `void` return functions do not return any value.
  - Must not have any `return` statements with values.

### Statement

    <stat>          ::= <stat-one>+
    <stat-one>      ::= `skip` `;`
                    |   `var` <ident> `:` <type> `=` <assign-rhs> `;`
                    |   <assign-lhs> `=` <assign-rhs> `;`
                    |   `scan` <assign-lhs> `;`
                    |   (`print` | `exit`) <expr> `;`
                    |   `return` <expr>? `;`
                    |   `if` <expr> `then` <stat> `else` <stat> `fi`
                    |   `while` <expr> `do` <stat> `done`
                    |   `break` `;` | `continue` `;`
                    |   `do` <stat> `done`
    <assign-lhs>    ::= <ident> <array-access>*
    <assign-rhs>    ::= <expr>
                    |   <array-liter>

### Type

    <type>      ::= <base-type> (`[` `]`)*
    <base-type> ::= `int`  | `int8`  | `int16`  | `int32`  | `int64`
                |   `uint` | `uint8` | `uint16` | `uint32` | `uint64`
                |   `bool`
                |   `char` | `string`

- Both `int` and `uint` types are 32-bit integers.
- All `int*` types are represented in two's complement form.
- All arrays are allocated on the stack i.e. no heap manipulation.
- Array of arrays internally use pointers.

### Expression

    <expr>          ::= <expr'> (<binary-op> <expr'>)*
    <expr'>         ::= <int-liter> | <bool-liter> | <char-liter> | <str-liter>
                    |   <ident> (<array-access>* | <arg-list>?)
                    |   <unary-op> <expr>
                    |   `(` <expr> `)`
    <arg-list>      ::= `(` (<expr> (`,` <expr>)*)? `)`
    <array-access>  ::= `[` <expr> `]`
    <unary-op>      ::= `-` | `not` | `len` | `ord` | `chr`
    <binary-op>     ::= `*` | `/` | `+` | `-` | `mod` | `>` | `>=` | `<` | `<=`
                    |   `==` | `!=` | `and` | `or`

### Lexeme

    <ident>         ::= (`_` | a-zA-Z) (`_` | a-zA-Z0-9)*
    <int-liter>     ::= (`+` | `-`)? ( `0b` (0-1)+
                                     | `0x` (0-9a-fA-F)+
                                     | (0-9)+)
    <bool-liter>    ::= `true` | `false`
    <char-liter>    ::= `'` <char> `'`
    <str-liter>     ::= `"` <char>* `"`
    <char>          ::= any-ASCII-except-`\`-`'`-`"` | `\` <esc-char>
    <esc-char>      ::= `0` | `t` | `n` | `r` | `"` | `'` | `\`
    <array-liter>   ::= `[` (<expr> (`,` <expr>)*)? `]`
    <comment>       ::= `#` any-char-except-EOL* <EOL>
