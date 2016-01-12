A ppx rewriter for monadic and applicative let bindings and match statements.

Overview
--------

The aim of this rewriter is to make monadic and applicative code look
nicer by writing custom binders the same way that we normally bind
variables. In OCaml, the common way to bind the result of a
computation to a variable is:

```ocaml
let VAR = EXPR in BODY
```

ppx\_let simply adds two new binders: `let%bind` and
`let%map`. These are rewritten into calls to the `bind` and
`map` functions respectively.  These functions are expected to have

```ocaml
val map : 'a t -> f:('a -> 'b) -> 'b t
val bind : 'a t -> ('a -> 'b t) -> 'b t
```

for some type `t`, as one might expect.

These functions are to be provided by the user, and are generally
expected to be part of the signatures of monads and applicatives
modules.  This is the case for all monads and applicatives defined by
the Jane Street's Core suite of libraries. (see the section below on
getting the right names into scope).

### Parallel bindings

ppx\_monad understands parallel bindings as well. i.e.:

```ocaml
let%bind VAR1 = EXPR1 and VAR2 = EXPR2 and VAR3 = EXPR3 in BODY
```

The `and` keyword is seen as a binding combination operator. To do so
it expects the presence of a `both` function, that lifts the OCaml
pair operation to the type `t` in question:

```ocaml
val both : 'a t -> 'b t -> ('a * 'b) t
```

### Match statements

We found that this form was quite useful for match statements as
well. So for convenience ppx\_monad also accept `%bind` and `%map` on
the `match` keyword. Morally `match%bind expr with cases` is seen as
`let%bind x = expr in match x with cases`.

Syntactic forms and actual rewriting
------------------------------------

`ppx_let` adds four syntactic forms

```ocaml
let%bind P = M in E

let%map P = M in E

match%bind M with P1 -> E1 | P2 -> E2 | ...

match%map M with P1 -> E1 | P2 -> E2 | ...
```

that expand into

```ocaml
bind M (fun P -> E)

map M (fun P -> E)

bind M (function P1 -> E1 | P2 -> E2 | ...)

map M (function P1 -> E1 | P2 -> E2 | ...)
```

respectively.

As with `let`, `let%bind` and `let%map` also support multiple
*parallel* bindings via the `and` keyword:

```ocaml
let%bind P1 = M1 and P2 = M2 and P3 = M3 and P4 = M4 in E

let%map P1 = M1 and P2 = M2 and P3 = M3 and P4 = M4 in E
```

that expand into

```ocaml
let x1 = M1 and x2 = M2 and x3 = M3 and x4 = M4 in
bind
  (both x1 (both x2 (both x3 x4)))
  (fun (P1, (P2, (P3, P4))) -> E)

let x1 = M1 and x2 = M2 and x3 = M3 and x4 = M4 in
map
  (both x1 (both x2 (both x3 x4)))
  (fun (P1, (P2, (P3, P4))) -> E)
```

respectively. (Instead of `x1`, `x2`, ... ppx\_monad uses
variable names that are unlikely to clash with other names)

As with `let`, names introduced by left-hand sides of the let bindings
are not available in subsequent right-hand sides of the same sequence.

Getting the right names in scope
--------------------------------

The description of how the `%bind` and `%map` syntax extensions expand
left out the fact that the names `bind`, `map`, `both`, and `return`
are not used directly, but rather qualified by `Let_syntax`.  For
example, we use `Let_syntax.bind` rather than merely `bind`.  This
means one just needs to get a properly loaded `Let_syntax` module in
scope to use `%bind` and `%map`.

For monads, `Core.Std.Monad.Make` produces a submodule `Let_syntax` of
the appropriate form.

For applicatives.  The convention for these modules is to have a
submodule `Let_syntax` of the form

```ocaml
module Let_syntax : sig
  val return : 'a -> 'a t
  val map    : 'a t -> f:('a -> 'b) -> 'b t
  val both   : 'a t -> 'b t -> ('a * 'b) t
  module Open_on_rhs : << some signature >>
  module Open_in_body : << some signature >>
end
```

The `Open_on_rhs` and `Open_in_body` submodules are used by variants
of `%map` and `%bind` called `%map_open` and `%bind_open`.

The `Open_on_rhs` submodule is locally opened on the right hand sides
of the rewritten let bindings in `%map_open` and `%bind_open`
expressions.  This is useful when programming with applicatives, which
operate in a staged manner where the operators used to construct the
applicatives are distinct from the operators used to manipulate the
values those applicatives produce.  For monads, `Open_on_rhs` contains
`return`.

The `Open_in_body` submodule is locally opened in the body of either a
`let%map_open` or `let%bind_open`.  It is often empty for
applicatives.  For monads in `Core`, it contains `return`.

For `match%map_open` and `match%bind_open` expressions, `Open_on_rhs`
is opened for the expression being matched on, and `Open_in_body` is
opened in the body of each pattern match clause.
