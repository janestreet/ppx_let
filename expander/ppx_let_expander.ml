open Base
open Ppxlib
open Ast_builder.Default

let pexp_let ~loc rec_ bindings e =
  match bindings with
  | [] -> e
  | _ :: _ -> pexp_let ~loc rec_ bindings e
;;

module List = struct
  include List

  let reduce_exn l ~f =
    match l with
    | [] -> invalid_arg "List.reduce_exn"
    | hd :: tl -> fold_left tl ~init:hd ~f
  ;;
end

module Extension_kind = struct
  type t =
    { do_open : bool
    ; collapse_binds : bool
    }

  let default = { do_open = false; collapse_binds = false }
  let default_open = { do_open = true; collapse_binds = false }
  let n = { do_open = false; collapse_binds = true }
  let n_open = { do_open = true; collapse_binds = true }
end

module Locality = struct
  type t =
    { allocate_function_on_stack : bool
    ; return_value_in_exclave : bool
    }

  let global = { allocate_function_on_stack = false; return_value_in_exclave = false }

  let all =
    List.Cartesian_product.map2
      Bool.all
      Bool.all
      ~f:(fun allocate_function_on_stack return_value_in_exclave ->
        { allocate_function_on_stack; return_value_in_exclave })
  ;;
end

module With_location = struct
  type t =
    | No_location
    | Location_of_callsite
    | Location_in_scope of string
end

module type Ext = sig
  (* The base string of all the related extensions. For example, if the value
     is "bind", then other extensions will include "bind_open", "bindn", and
     "bindn_open" - all of which start with "bind" *)
  val name : string
  val with_location : With_location.t
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
    -> locality:Locality.t
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

let wrap_expansion_identity ~loc ~modul:_ bindings expression ~expand =
  expand ~loc bindings expression
;;

(* Wrap a function body in [exclave_] *)
let wrap_exclave ~loc expr = [%expr [%e expr]]

let maybe_wrap_exclave ~loc ~return_value_in_exclave expr =
  match return_value_in_exclave with
  | false -> expr
  | true -> wrap_exclave ~loc expr
;;

let maybe_wrap_local ~loc ~allocate_function_on_stack func =
  if allocate_function_on_stack then [%expr [%e func]] else func
;;

type t = (module Ext)

let ext_full_name (module Ext : Ext) ~(locality : Locality.t) (kind : Extension_kind.t) =
  let { allocate_function_on_stack; return_value_in_exclave } : Locality.t = locality in
  String.concat
    [ Ext.name
    ; (if kind.collapse_binds then "n" else "")
    ; (match allocate_function_on_stack, return_value_in_exclave with
       | true, true -> "l" (* "local" *)
       | true, false -> "l_fun" (* "local function" *)
       | false, true -> "l_val" (* "local value" *)
       | false, false -> "")
    ; (if kind.do_open then "_open" else "")
    ]
;;

let let_syntax = "Let_syntax"

let let_syntax ~modul : Longident.t =
  match modul with
  | None -> Lident let_syntax
  | Some id -> Ldot (Ldot (id.txt, let_syntax), let_syntax)
;;

let open_on_rhs ~loc ~modul =
  pmod_ident ~loc (Located.mk ~loc (Longident.Ldot (let_syntax ~modul, "Open_on_rhs")))
;;

let eoperator ~loc ~modul func =
  let lid : Longident.t = Ldot (let_syntax ~modul, func) in
  pexp_ident ~loc (Located.mk ~loc lid)
;;

let qualified_return ~loc ~modul expr =
  pexp_apply ~loc (eoperator ~loc ~modul "return") [ Nolabel, expr ]
;;

let location_arg ~loc = Labelled "here", Ppx_here_expander.lift_position ~loc
let location_arg_in_scope ~loc name = Labelled "here", evar ~loc name

let nontail ~loc expr =
  let attr = attribute ~loc ~name:{ txt = "nontail"; loc } ~payload:(PStr []) in
  { expr with pexp_attributes = attr :: expr.pexp_attributes }
;;

let bind_apply
  ?(fn_label = "f")
  ~prevent_tail_call
  ~op_name
  ~loc
  ~modul
  ~with_location
  ~arg
  ~fn
  ()
  =
  let location_args =
    match (with_location : With_location.t) with
    | No_location -> []
    | Location_of_callsite -> [ location_arg ~loc ]
    | Location_in_scope name -> [ location_arg_in_scope ~loc name ]
  in
  let args = location_args @ [ Nolabel, arg; Labelled fn_label, fn ] in
  let expr = pexp_apply ~loc (eoperator ~loc ~modul op_name) args in
  if prevent_tail_call then nontail ~loc expr else expr
;;

let do_not_enter_value vb =
  let loc = vb.pvb_loc in
  let attr =
    { attr_loc = loc
    ; attr_name = { loc; txt = Attribute.name Ast_traverse.do_not_enter_value_binding }
    ; attr_payload = PStr []
    }
  in
  { vb with pvb_attributes = attr :: vb.pvb_attributes }
;;

let expand_with_tmp_vars ~loc bindings expr ~f =
  match bindings with
  | [ _ ] -> f ~loc bindings expr
  | _ ->
    (* s/rhs/tmp_var and s/lhs/tmp_var *)
    let s_rhs_tmp_var, s_lhs_tmp_var =
      List.map bindings ~f:(fun vb ->
        let var = gen_symbol ~prefix:"__let_syntax" () in
        let loc = { vb.pvb_expr.pexp_loc with loc_ghost = true } in
        let rhs = { vb with pvb_expr = evar ~loc var } in
        let lhs =
          do_not_enter_value
            { vb with
              pvb_pat = pvar ~loc var
            ; pvb_loc = { vb.pvb_loc with loc_ghost = true }
            }
        in
        rhs, lhs)
      |> List.unzip
    in
    pexp_let ~loc Nonrecursive s_lhs_tmp_var (f ~loc s_rhs_tmp_var expr)
;;

let maybe_destruct ~destruct ~loc ~modul ~return_value_in_exclave ~lhs ~body =
  let whole_value_var = gen_symbol ~prefix:"__pattern_syntax" () in
  let whole_value_pattern = ppat_var ~loc { txt = whole_value_var; loc } in
  let whole_value_expr = pexp_ident ~loc { txt = Lident whole_value_var; loc } in
  match destruct ~assume_exhaustive:true ~loc ~modul ~lhs ~rhs:whole_value_expr ~body with
  | Some destruction ->
    maybe_wrap_exclave ~loc ~return_value_in_exclave destruction
    |> pexp_fun ~loc Nolabel None whole_value_pattern
  | None ->
    maybe_wrap_exclave ~loc ~return_value_in_exclave body
    |> pexp_fun ~loc Nolabel None lhs
;;

let expand_letn
  (module Ext : Ext)
  ~loc
  ~modul
  ~locality:({ return_value_in_exclave; allocate_function_on_stack } : Locality.t)
  bindings
  body
  =
  let n = List.length bindings in
  let operator =
    match n with
    | 1 -> eoperator ~loc ~modul Ext.name
    | n -> eoperator ~loc ~modul (Printf.sprintf "%s%d" Ext.name n)
  in
  let bindings_args =
    bindings |> List.map ~f:(fun { pvb_expr; _ } -> Nolabel, pvb_expr)
  in
  let func =
    List.fold_right
      bindings
      ~init:(maybe_wrap_exclave ~loc ~return_value_in_exclave body)
      ~f:(fun { pvb_pat; _ } lower ->
        maybe_destruct
          ~destruct:Ext.destruct
          ~modul
          ~return_value_in_exclave:false
          ~loc
          ~lhs:pvb_pat
          ~body:lower)
    |> maybe_wrap_local ~loc ~allocate_function_on_stack
  in
  let location_args =
    match Ext.with_location with
    | No_location -> []
    | Location_of_callsite -> [ location_arg ~loc ]
    | Location_in_scope name -> [ location_arg_in_scope ~loc name ]
  in
  let args = bindings_args @ location_args @ [ Labelled "f", func ] in
  let expr = pexp_apply ~loc operator args in
  if allocate_function_on_stack then nontail ~loc expr else expr
;;

let maybe_open ~(extension_kind : Extension_kind.t) ~to_open:module_to_open expr =
  let loc = { expr.pexp_loc with loc_ghost = true } in
  if extension_kind.do_open
  then
    pexp_open ~loc (open_infos ~loc ~override:Override ~expr:(module_to_open ~loc)) expr
  else expr
;;

let expand_let
  (module Ext : Ext)
  ~loc
  ~modul
  ~locality:({ allocate_function_on_stack; return_value_in_exclave } : Locality.t)
  bindings
  body
  =
  if List.length bindings = 0
  then invalid_arg "expand_let: list of bindings must be non-empty";
  (* Build expression [both E1 (both E2 (both ...))] *)
  let nested_boths =
    let rev_boths = List.rev_map bindings ~f:(fun vb -> vb.pvb_expr) in
    List.reduce_exn rev_boths ~f:(fun acc e ->
      let loc = { e.pexp_loc with loc_ghost = true } in
      eapply ~loc (eoperator ~loc ~modul "both") [ e; acc ])
  in
  (* Build pattern [(P1, (P2, ...))] *)
  let nested_patterns =
    let rev_patts = List.rev_map bindings ~f:(fun vb -> vb.pvb_pat) in
    let min_position, max_position =
      match rev_patts with
      | hd :: tl ->
        let init = hd.ppat_loc.loc_start, hd.ppat_loc.loc_end in
        List.fold ~init tl ~f:(fun (min, max) pattern ->
          ( Location.min_pos pattern.ppat_loc.loc_start min
          , Location.max_pos pattern.ppat_loc.loc_end max ))
      | [] -> assert false
    in
    let tuple_loc =
      { loc_start = min_position; loc_end = max_position; loc_ghost = true }
    in
    List.reduce_exn rev_patts ~f:(fun acc p -> ppat_tuple ~loc:tuple_loc [ p; acc ])
  in
  let fn =
    maybe_destruct
      ~destruct:Ext.destruct
      ~loc
      ~modul
      ~return_value_in_exclave
      ~lhs:nested_patterns
      ~body
    |> maybe_wrap_local ~loc ~allocate_function_on_stack
  in
  bind_apply
    ~op_name:Ext.name
    ~loc
    ~modul
    ~with_location:Ext.with_location
    ~arg:nested_boths
    ~fn
    ~prevent_tail_call:(Ext.prevent_tail_call || allocate_function_on_stack)
    ()
;;

let expand_match (module Ext : Ext) ~extension_kind ~loc ~modul ~locality expr cases =
  let expr = maybe_open ~extension_kind ~to_open:(open_on_rhs ~modul) expr in
  Ext.expand_match ~loc ~modul ~locality expr cases
;;

let expand_if t ~extension_kind ~loc ~modul ~locality expr then_ else_ =
  expand_match
    t
    ~extension_kind
    ~loc
    ~modul
    ~locality
    expr
    [ case ~lhs:(pbool ~loc true) ~guard:None ~rhs:then_
    ; case ~lhs:(pbool ~loc false) ~guard:None ~rhs:else_
    ]
;;

let expand_while
  (module Ext : Ext)
  ~locality:
    ({ allocate_function_on_stack; return_value_in_exclave } as locality : Locality.t)
  ~extension_kind
  ~loc
  ~modul
  ~cond
  ~body
  =
  let loop_name = gen_symbol ~prefix:"__let_syntax_loop" () in
  let ploop = pvar ~loc loop_name in
  let eloop = evar ~loc loop_name in
  let loop_call = pexp_apply ~loc eloop [ Nolabel, eunit ~loc ] in
  let loop_body =
    let then_ =
      bind_apply
        ~op_name:Ext.name
        ~loc
        ~modul
        ~with_location:Ext.with_location
        ~arg:body
        ~fn:eloop
        ~prevent_tail_call:(Ext.prevent_tail_call || allocate_function_on_stack)
        ()
    in
    let else_ = qualified_return ~loc ~modul (eunit ~loc) in
    expand_if (module Ext) ~extension_kind ~modul ~locality ~loc cond then_ else_
  in
  let loop_body = maybe_wrap_exclave ~loc ~return_value_in_exclave loop_body in
  let loop_func = pexp_fun ~loc Nolabel None (punit ~loc) loop_body in
  pexp_let
    ~loc
    Recursive
    [ do_not_enter_value (value_binding ~loc ~pat:ploop ~expr:loop_func) ]
    loop_call
;;

let expand_function ~loc ~return_value_in_exclave cases =
  match return_value_in_exclave with
  | false -> pexp_function_cases ~loc cases
  | true ->
    let var = gen_symbol ~prefix:"__let_syntax" () in
    pexp_match ~loc (evar ~loc var) cases
    |> wrap_exclave ~loc
    |> pexp_fun ~loc Nolabel None (pvar ~loc var)
;;

module Map : Ext = struct
  let name = "map"
  let with_location = With_location.No_location
  let wrap_expansion = wrap_expansion_identity
  let prevent_tail_call = false

  let disallow_expression _ = function
    | Pexp_while (_, _) -> Error "while%%map is not supported. use while%%bind instead."
    | _ -> Ok ()
  ;;

  let destruct ~assume_exhaustive:_ ~loc:_ ~modul:_ ~lhs:_ ~rhs:_ ~body:_ = None

  let expand_match
    ~loc
    ~modul
    ~locality:({ allocate_function_on_stack; return_value_in_exclave } : Locality.t)
    expr
    cases
    =
    bind_apply
      ~loc
      ~modul
      ~with_location
      ~op_name:name
      ~arg:expr
      ~fn:(expand_function ~loc ~return_value_in_exclave cases)
      ~prevent_tail_call:allocate_function_on_stack
      ()
  ;;
end

module Bind : Ext = struct
  let name = "bind"
  let with_location = With_location.No_location
  let wrap_expansion = wrap_expansion_identity
  let prevent_tail_call = false

  let disallow_expression (extension_kind : Extension_kind.t) = function
    | Pexp_while (_, _) when extension_kind.collapse_binds ->
      Error "while%%bindn is not supported. use while%%bind instead."
    | _ -> Ok ()
  ;;

  let destruct ~assume_exhaustive:_ ~loc:_ ~modul:_ ~lhs:_ ~rhs:_ ~body:_ = None

  let expand_match
    ~loc
    ~modul
    ~locality:({ allocate_function_on_stack; return_value_in_exclave } : Locality.t)
    expr
    cases
    =
    bind_apply
      ~loc
      ~modul
      ~with_location
      ~op_name:name
      ~arg:expr
      ~fn:(expand_function ~loc ~return_value_in_exclave cases)
      ~prevent_tail_call:allocate_function_on_stack
      ()
  ;;
end

let variables_of =
  object
    inherit [string Ppxlib.loc list] Ast_traverse.fold as super

    method! pattern p acc =
      let acc = super#pattern p acc in
      match p.ppat_desc with
      | Ppat_var var -> var :: acc
      | Ppat_alias (_, var) -> var :: acc
      | _ -> acc
  end
;;

let pattern_variables pattern =
  List.dedup_and_sort
    ~compare:(fun x y -> String.compare x.txt y.txt)
    (variables_of#pattern pattern [])
;;

let maybe_enter_value pat expr =
  match pattern_variables pat with
  | [ { loc; txt } ] ->
    let loc = { loc with loc_ghost = true } in
    let attr =
      { attr_loc = loc
      ; attr_name = { loc; txt = Attribute.name Ast_traverse.enter_value }
      ; attr_payload = PStr [ pstr_eval ~loc (evar ~loc txt) [] ]
      }
    in
    { expr with pexp_attributes = attr :: expr.pexp_attributes }
  | [] | _ :: _ :: _ -> expr
;;

let raise_unhandled_expression ~loc name =
  Location.raise_errorf
    ~loc
    "'%%%s' can only be used with 'let', 'match', 'while', 'function', and 'if'"
    name
;;

let expand ((module Ext : Ext) as ext) extension_kind ~modul ~locality expr =
  let loc = { expr.pexp_loc with loc_ghost = true } in
  let expansion =
    let expr_desc =
      match Ext.disallow_expression extension_kind expr.pexp_desc with
      | Error message -> Location.raise_errorf ~loc "%s" message
      | Ok () -> expr.pexp_desc
    in
    match Ppxlib_jane.Shim.Expression_desc.of_parsetree expr_desc ~loc with
    | Pexp_let (Nonrecursive, bindings, expr) ->
      let bindings =
        List.map bindings ~f:(fun vb ->
          let pvb_pat, pvb_expr =
            (* Temporary hack tentatively detecting that the parser
               has expanded `let x : t = e` into `let x : t = (e : t)`.

               For reference, here is the relevant part of the parser:
               https://github.com/ocaml/ocaml/blob/4.07/parsing/parser.mly#L1628 *)
            match
              ( Ppxlib_jane.Shim.Pattern_desc.of_parsetree vb.pvb_pat.ppat_desc
              , Ppxlib_jane.Shim.Expression_desc.of_parsetree
                  ~loc:vb.pvb_expr.pexp_loc
                  vb.pvb_expr.pexp_desc )
            with
            | ( Ppat_constraint (p, Some { ptyp_desc = Ptyp_poly ([], t1); _ }, _)
              , Pexp_constraint (_, Some t2, _) )
              when phys_equal t1 t2 || Poly.equal t1 t2 ->
              ( p
              , { vb.pvb_expr with
                  pexp_loc = { vb.pvb_expr.pexp_loc with loc_ghost = true }
                } )
            | _ -> vb.pvb_pat, vb.pvb_expr
          in
          { vb with
            pvb_pat
          ; pvb_expr =
              maybe_open
                ~extension_kind
                ~to_open:(open_on_rhs ~modul)
                (maybe_enter_value pvb_pat pvb_expr)
          })
      in
      let f ~loc value_bindings expression =
        let expand =
          if extension_kind.collapse_binds
          then expand_letn ext ~modul ~locality
          else expand_let ext ~modul ~locality
        in
        Ext.wrap_expansion ~loc ~modul value_bindings expression ~expand
      in
      expand_with_tmp_vars ~loc bindings expr ~f
    | Pexp_let (Recursive, _, _) ->
      let ext_full_name = ext_full_name ext ~locality extension_kind in
      Location.raise_errorf ~loc "'let%%%s' may not be recursive" ext_full_name
    | Pexp_match (expr, cases) ->
      expand_match ext ~extension_kind ~loc ~modul ~locality expr cases
    | Pexp_ifthenelse (expr, then_, else_) ->
      let else_ =
        match else_ with
        | Some else_ -> else_
        | None ->
          let ext_full_name = ext_full_name ext ~locality extension_kind in
          Location.raise_errorf ~loc "'if%%%s' must include an else branch" ext_full_name
      in
      expand_if ext ~extension_kind ~loc ~modul ~locality expr then_ else_
    | Pexp_while (cond, body) ->
      expand_while ext ~extension_kind ~loc ~modul ~locality ~cond ~body
    | Pexp_function (params, constraint_, body) ->
      (match
         Ppxlib_jane.Legacy_pexp_function.of_pexp_function ~params ~constraint_ ~body
       with
       | Legacy_pexp_function cases ->
         let temp_var = gen_symbol ~prefix:"__let_syntax" () in
         let temp_pattern = ppat_var ~loc { txt = temp_var; loc } in
         let temp_expr = pexp_ident ~loc { txt = Lident temp_var; loc } in
         let match_expr =
           expand_match ext ~extension_kind ~loc ~modul ~locality temp_expr cases
         in
         pexp_fun ~loc Nolabel None temp_pattern match_expr
       | Legacy_pexp_fun _ | Legacy_pexp_newtype _ ->
         raise_unhandled_expression ~loc (ext_full_name ext ~locality extension_kind))
    | _ -> raise_unhandled_expression ~loc (ext_full_name ext ~locality extension_kind)
  in
  { expansion with pexp_attributes = expr.pexp_attributes @ expansion.pexp_attributes }
;;

let map = (module Map : Ext)
let bind = (module Bind : Ext)
