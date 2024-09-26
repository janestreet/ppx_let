open! Core

type t = Ppx_let_expander.Locality.t =
  { allocate_function_on_stack : bool
  ; return_value_in_exclave : bool
  }
[@@deriving enumerate, sexp_of]

val global : t
