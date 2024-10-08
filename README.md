ppx_let
=======

A ppx rewriter for monadic and applicative let bindings, match expressions, and
if expressions.

Overview
--------

The aim of this rewriter is to make monadic and applicative code look nicer by
writing custom binders the same way that we normally bind variables. In OCaml,
the common way to bind the result of a computation to a variable is:

```ocaml
let VAR = EXPR in BODY
```

ppx\_let simply adds two new binders: `let%bind` and `let%map`. These are
rewritten into calls to the `bind` and `map` functions respectively. These
functions are expected to have

```ocaml
val map  : 'a t -> f:('a -> 'b)   -> 'b t
val bind : 'a t -> f:('a -> 'b t) -> 'b t
```

for some type `t`, as one might expect.

These functions are to be provided by the user, and are generally expected to be
part of the signatures of monads and applicatives modules. This is the case for
all monads and applicatives defined by the Jane Street's Core suite of
libraries. (see the section below on getting the right names into scope).

### Parallel bindings

ppx\_let understands parallel bindings as well. i.e.:

```ocaml
let%bind VAR1 = EXPR1 and VAR2 = EXPR2 and VAR3 = EXPR3 in BODY
```

The `and` keyword is seen as a binding combination operator. To do so it expects
the presence of a `both` function, that lifts the OCaml pair operation to the
type `t` in question:

```ocaml
val both : 'a t -> 'b t -> ('a * 'b) t
```

Some applicatives have optimized `map` functions for more than two arguments.
These applicatives will export functions like `map4` shown below:

```ocaml
val map4: 'a t -> 'b t -> 'c t -> 'd t -> f:('a -> 'b -> 'c -> 'd -> 'r) -> 'r t
```

In order to use these optmized functions, ppx\_let provides the `let%mapn`
syntax, which picks the right `map{n}` function to call based on the amount of
applicatives bound by the syntax.

### Match statements

We found that this form was quite useful for match statements as well. So for
convenience ppx\_let also accepts `%bind` and `%map` on the `match` keyword.
Morally `match%bind expr with cases` is seen as `let%bind x = expr in match x
with cases`.

### If statements

As a further convenience, ppx\_let accepts `%bind` and `%map` on the `if`
keyword. The expression `if%bind expr1 then expr2 else expr3` is morally
equivalent to `let%bind p = expr1 in if p then expr2 else expr3`.

### Function statements

We accept `function%bind` and `function%map` too.

```ocaml
let f = function%bind
  | Some a -> g a
  | None -> h
```

is equivalent to

```ocaml
let f = fun temp ->
  match%bind temp with
  | Some a -> g a
  | None -> h
```

### While statements

We also expand `while%bind expr1 do expr2 done` as

```ocaml
let rec loop () =
  if%bind expr1
  then (
    let%bind () = expr2 in
    loop ())
  else return ()
in loop ()
```

Note that this form will (potentially) evaluate the textual form of
expr1 multiple times!

We do not support `while%map`, as that cannot be implemented without
`bind`.

Syntactic forms and actual rewriting
------------------------------------

`ppx_let` adds seven syntactic forms

```ocaml
let%bind P = M in E

let%map  P = M in E

match%bind M with P1 -> E1 | P2 -> E2 | ...

match%map  M with P1 -> E1 | P2 -> E2 | ...

if%bind M then E1 else E2

if%map  M then E1 else E2

while%bind M do E done
```

that expand into

```ocaml
bind M ~f:(fun P -> E)

map  M ~f:(fun P -> E)

bind M ~f:(function P1 -> E1 | P2 -> E2 | ...)

map  M ~f:(function P1 -> E1 | P2 -> E2 | ...)

bind M ~f:(function true -> E1 | false -> E2)

map  M ~f:(function true -> E1 | false -> E2)

let rec loop () = bind M ~f:(function true -> bind E ~f:loop | false -> return ()) in loop ()
```

respectively.

As with `let`, `let%bind` and `let%map` also support multiple *parallel*
bindings via the `and` keyword:

```ocaml
let%bind P1 = M1 and P2 = M2 and P3 = M3 and P4 = M4 in E

let%map  P1 = M1 and P2 = M2 and P3 = M3 and P4 = M4 in E
```

that expand into

```ocaml
let x1 = M1 and x2 = M2 and x3 = M3 and x4 = M4 in
bind
  (both x1 (both x2 (both x3 x4)))
  ~f:(fun (P1, (P2, (P3, P4))) -> E)

let x1 = M1 and x2 = M2 and x3 = M3 and x4 = M4 in
map
  (both x1 (both x2 (both x3 x4)))
  ~f:(fun (P1, (P2, (P3, P4))) -> E)
```

respectively. (Instead of `x1`, `x2`, ... ppx\_let uses variable names that are
unlikely to clash with other names)

As with `let`, names introduced by left-hand sides of the let bindings are not
available in subsequent right-hand sides of the same sequence.

Getting the right names in scope
--------------------------------

The description of how the `%bind` and `%map` syntax extensions expand left out
the fact that the names `bind`, `map`, `both`, and `return` are not used
directly., but rather qualified by `Let_syntax`. For example, we use
`Let_syntax.bind` rather than merely `bind`.

This means one just needs to get a properly loaded `Let_syntax` module
in scope to use `%bind` and `%map`. The intended way to do this is to
create a module `Let_syntax` with a signature like:
```ocaml
module Let_syntax : sig
  module Let_syntax : sig
    val bind : ...
    val map : ...
    ...
  end
  ...
end
```
and then use `open Let_syntax` to make the inner `Let_syntax` module
available.

Alternatively, the extension can use values from a `Let_syntax` module
other than the one in scope. If you write `%map.A.B.C` instead of
`%map`, the expansion will use `A.B.C.Let_syntax.Let_syntax.map`
instead of `Let_syntax.map` (and similarly for all extension points).

For monads, `Core.Monad.Make` produces a submodule `Let_syntax` of the
appropriate form.

For applicatives, the convention for these modules is to have a submodule
`Let_syntax` of the form:

```ocaml
module Let_syntax : sig
  module Let_syntax : sig
    val return : 'a -> 'a t
    val map    : 'a t -> f:('a -> 'b) -> 'b t
    val both   : 'a t -> 'b t -> ('a * 'b) t
    module Open_on_rhs : << some signature >>
  end
end
```

The `Open_on_rhs` submodule is used by variants of `%map` and `%bind` called
`%map_open` and `%bind_open`. It is locally opened on the right hand sides of
the rewritten let bindings in `%map_open` and `%bind_open` expressions. For
`match%map_open` and `match%bind_open` expressions, `Open_on_rhs` is opened for
the expression being matched on.

`Open_on_rhs` is useful when programming with applicatives, which operate in a
staged manner where the operators used to construct the applicatives are
distinct from the operators used to manipulate the values those applicatives
produce. For monads, `Open_on_rhs` contains `return`.

Local-aware let syntax
------------

`ppx_let` can operate on local values. This requires [a compiler that supports the
`local_` and `exclave_` keywords, and stack
allocation](https://github.com/ocaml-flambda/flambda-backend). (The Jane Street branch of
the compiler supports these.) Several variants of the `%map` and `%bind` extensions allow
use of local expressions in different contexts. The differences are best demonstrated by
example, showing what each extension expands to.

### Example: `%mapl_fun` and `%bindl_fun`

The closure generated by the ppx will be stack-allocated:
```ocaml
(* Using ppx *)
let open Option.Let_syntax in
let%mapl_fun x = y in
...
;;

(* Expansion *)
let open Option.Let_syntax in
Let_syntax.map y ~f:(local_ fun x -> ...) [@nontail]
```

You can use this with any `Let_syntax` whose `map`/`bind` takes its
function argument locally, e.g.:

```ocaml
val bind : 'a t -> local_ ('a -> 'b t) -> 'b t
```

### Example: `%mapl_val` and `%bindl_val`

The generated closure will not have its own region, and thus the return value may be
stack allocated:
```ocaml
(* Using ppx *)
let open Local_option.Let_syntax in
let%mapl_val x = y in
...
;;

(* Expansion *)
let open Local_option.Let_syntax in
Let_syntax.map y ~f:(fun x -> exclave_ ...)
```

You can use with any `Let_syntax` module that requires the function
argument to return a local value, e.g.:

```ocaml
val bind : 'a t -> ('a -> local_ 'b t) -> local_ 'b t
```

### Example: `%mapl` and `%bindl`

Combines the effects of `%mapl_fun/bindl_fun` and `%mapl_val/%bindl_val`. The closure
generated by the ppx will be stack allocated _and_ the generated closure will not have its
own region. Thus, the return value may be stack allocated.

```ocaml
(* Using ppx *)
let open Local_option.Let_syntax in
let%mapl x = y in
...
;;

(* Expansion *)
let open Local_option.Let_syntax in
Let_syntax.map y ~f:(local_ fun x -> exclave_ ...) [@nontail]
```

You can use use this with any `Let_syntax` module that requires the function
argument to return a local value and that takes its function argument
locally, e.g.

```ocaml
val bind : 'a t -> local_ ('a -> local_ 'b t) -> local_ 'b t
```

### List of local-aware let syntaxes

| Let syntax                | Ensures the function is stack allocated? | Allow the function to return a stack-allocated value? |
|---------------------------|------------------------------------------|-------------------------------------------------------|
| `%map`, `%bind`           | No                                       | No                                                    |
| `%mapl`, `%bindl`         | Yes                                      | Yes                                                   |
| `%mapl_fun`, `%bindl_fun` | Yes                                      | No                                                    |
| `%mapl_val`, `%bindl_val` | No                                       | Yes                                                   |

You can use these extensions with `%mapn` and `%bindn` as well. The full list of such
extensions is : `%mapnl`, `%bindnl`, `%mapnl_fun`, `%bindnl_fun`, `%mapnl_val`, and
`%bindnl_val`.
