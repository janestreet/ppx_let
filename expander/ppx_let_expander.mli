open Ppxlib

module Extension_kind : sig
  type t =
    { do_open : bool
    ; collapse_binds : bool
    }

  (* let%bind, let%map, etc. *)
  val default : t

  (* let%bind_open, let%map_open, etc. *)
  val default_open : t

  (* let%bindn, let%mapn, etc. *)
  val n : t

  (* let%bindn_open, let%mapn_open, etc. *)
  val n_open : t
end

module type Ext = sig
  (* The base string of all the related extensions. For example, if the value
     is "bind", then other extensions will include "bind_open", "bindn", and
     "bindn_open" - all of which start with "bind" *)
  val name : string
  val with_location : bool

  (* When true, prevent_tail_call will keep the resulting 
     function application from being in tail position by introducing a local 
     variable.  This is useful when working in with locals, and was added in order to 
     allow ppx_bonsai to transform 

     {[ 
       let%sub a = foo in 
       a
     ]}

     into 

     {[ ((sub foo ~f:(fun a -> a))[@nontail]) ]}

     instead of 

     {[ sub foo ~f:(fun a -> a) ]} *)
  val prevent_tail_call : bool

  (* Called before each expansion to ensure that the expression being expanded
     is supported. *)
  val disallow_expression : Extension_kind.t -> expression_desc -> (unit, string) Result.t

  (* Called when expanding a let-binding (and indirectly, when expanding a
     match-expression) to destructure [rhs]. The resulting expression should
     make each variable in [lhs] available for use in [body]. If the result is
     [None], then no special destructuring is necessary. *)
  val destruct
    :  assume_exhaustive:bool
    -> loc:location
    -> modul:longident loc option
    -> lhs:pattern
    -> rhs:expression
    -> body:expression
    -> expression option

  (* Expands any match%[name] expressions. It is also used when expanding
     if%[name]. *)
  val expand_match
    :  loc:location
    -> modul:longident loc option
    -> locality:[ `local | `global ]
    -> expression
    -> case list
    -> expression

  (* [expand] is the function that normally expands let%[name]. [wrap_expansion] can be
     used to change the parameters given to [expand] and can also tranform the output of
     [expand]. *)
  val wrap_expansion
    :  loc:location
    -> modul:longident loc option
    -> value_binding list
    -> expression
    -> expand:(loc:location -> value_binding list -> expression -> expression)
    -> expression
end

(* A trivial implementation of [Ext.wrap_expansion] that does nothing to change
   the expansion behavior. *)
val wrap_expansion_identity
  :  loc:location
  -> modul:longident loc option
  -> value_binding list
  -> expression
  -> expand:(loc:location -> value_binding list -> expression -> expression)
  -> expression

type t = (module Ext)

val ext_full_name : t -> locality:[ `local | `global ] -> Extension_kind.t -> label
val bind : t
val map : t
val variables_of : label loc list Ast_traverse.fold

module Map : sig
  val name : string
  val with_location : bool
end

val eoperator : loc:location -> modul:longident loc option -> label -> expression

val expand_match
  :  t
  -> extension_kind:Extension_kind.t
  -> loc:location
  -> modul:longident loc option
  -> locality:[ `local | `global ]
  -> expression
  -> case list
  -> expression

val maybe_destruct
  :  destruct:
       (assume_exhaustive:bool
        -> loc:location
        -> modul:'a
        -> lhs:pattern
        -> rhs:expression
        -> body:expression
        -> expression option)
  -> loc:location
  -> modul:'a
  -> locality:[ `local | `global ]
  -> lhs:pattern
  -> body:expression
  -> expression

val bind_apply
  :  ?fn_label:string (** default: "f" *)
  -> prevent_tail_call:bool
  -> op_name:label
  -> loc:location
  -> modul:longident loc option
  -> with_location:bool
  -> arg:expression
  -> fn:expression
  -> unit
  -> expression

val qualified_return
  :  loc:location
  -> modul:longident loc option
  -> expression
  -> expression

val expand
  :  t
  -> Extension_kind.t
  -> modul:longident loc option
  -> locality:[ `local | `global ]
  -> expression
  -> expression

val do_not_enter_value : value_binding -> value_binding
val nontail : loc:location -> expression -> expression
