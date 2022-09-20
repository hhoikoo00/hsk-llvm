## Future Features to implement

### Struct

    <struct-def>    ::= `struct` <ident> `is`
                            (<ident> `:` <type> `;`)+
                        `end`
    <assign-lhs>    ::= <ident> (<array-access> | `.` <ident>)*
    <assign-rhs>    ::= ...
                    |   <struct-liter>
    <struct-liter>  ::= `{` (`.` <ident> `=` <assign-rhs>)+ `}`
    <expr'>         ::= ...
                    |   <ident> <arg-list>? (<array-access> | `.` <ident>)*
    <type>          ::= ...
                    |   `struct` <ident>
    <program>       ::= <struct-def>* <func>*

- All structs must have at least one field.
- All members of a struct must have a unique identifier.
- `<expr'>`
  - `function_call()[array-access]` and `function_call().member-access`
    allowed, but not vice versa as function pointers are not implemented.
- Struct definitions introduced before functions.
- All struct types must have a unique identifier within the struct types.

### Modules

    <import>        ::= `import` (`*` | <import-list>) `from` `"` <filepath> `"` <EOL>
    <import-list>   ::= <ident> (`,` <ident>)*
    <program>       ::= <import>*
                        (`export`? <struct-def>)*
                        (`export`? <func>)*

- Struct definitions and functions can be imported.
- Filepath can be relative to source file directory or absolute (OS-dependent?)
- Imported files compiled before the parent source file.
- All structs and functions that can be exported must be explicitly marked as such.

### Pointer

    <type>      ::= <base-type> (`[` `]` | `*`)*
    <ref>       ::= `&`
    <deref>     ::= `*`
    ...
    <assign-lhs>    ::= <deref>* <ident> <array-access>*
    <unary-op>      ::= ... | <ref> | <deref>

- Identical to C-style semantics.

### New/Free

    <assign-rhs>    ::= ...
                    |   `new` <new-type> `with` <assign-rhs>
    <stat'>         ::= ...
                    |   `free` <assign-lhs>
    <new-type>      ::= <base-type> (`[` `]`)*
                    |   `struct` <ident>

- `new` returns pointer to heap-allocated data.
- No default values also for heap-alloc values.
  - Hence initial value must be specified with `with` clause.
- `free` any assignable variables (i.e. (nested) arrays, heap-alloc'ed struct members).
