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

module type Ext = sig
  (* The base string of all the related extensions. For example, if the value
     is "bind", then other extensions will include "bind_open", "bindn", and
     "bindn_open" - all of which start with "bind" *)
  val name : string

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
    -> expression
    -> case list
    -> expression
end

type t = (module Ext)

let ext_full_name (module Ext : Ext) { Extension_kind.do_open; collapse_binds; _ } =
  let result = Ext.name in
  let result = if collapse_binds then String.concat [ result; "n" ] else result in
  if do_open then String.concat [ result; "_open" ] else result
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

let bind_apply ~op_name ~loc ~modul ~arg ~fn =
  pexp_apply ~loc (eoperator ~loc ~modul op_name) [ Nolabel, arg; Labelled "f", fn ]
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
        let loc = { vb.pvb_pat.ppat_loc with loc_ghost = true } in
        let lhs =
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

let catch_all_case ~loc =
  case ~lhs:(ppat_any ~loc) ~guard:None ~rhs:(pexp_assert ~loc (ebool ~loc false))
;;

let maybe_destruct (module Ext : Ext) ~assume_exhaustive ~loc ~modul ~lhs ~body =
  let whole_value_var = gen_symbol ~prefix:"__pattern_syntax" () in
  let whole_value_pattern = ppat_var ~loc { txt = whole_value_var; loc } in
  let whole_value_expr = pexp_ident ~loc { txt = Lident whole_value_var; loc } in
  match Ext.destruct ~assume_exhaustive ~loc ~modul ~lhs ~rhs:whole_value_expr ~body with
  | Some destruction -> pexp_fun ~loc Nolabel None whole_value_pattern destruction
  | None ->
    if assume_exhaustive
    then pexp_fun ~loc Nolabel None lhs body
    else pexp_function ~loc [ case ~lhs ~guard:None ~rhs:body; catch_all_case ~loc ]
;;

let expand_letn (module Ext : Ext) ~assume_exhaustive ~loc ~modul bindings body =
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
    List.fold_right bindings ~init:body ~f:(fun { pvb_pat; _ } lower ->
      maybe_destruct
        (module Ext)
        ~assume_exhaustive
        ~modul
        ~loc
        ~lhs:pvb_pat
        ~body:lower)
  in
  let args = List.append bindings_args [ Labelled "f", func ] in
  pexp_apply ~loc operator args
;;

let maybe_open ~extension_kind ~to_open:module_to_open expr =
  let loc = { expr.pexp_loc with loc_ghost = true } in
  if extension_kind.Extension_kind.do_open
  then
    pexp_open ~loc (open_infos ~loc ~override:Override ~expr:(module_to_open ~loc)) expr
  else expr
;;

let expand_let (module Ext : Ext) ~assume_exhaustive ~loc ~modul bindings body =
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
    List.reduce_exn rev_patts ~f:(fun acc p ->
      let loc = { p.ppat_loc with loc_ghost = true } in
      ppat_tuple ~loc [ p; acc ])
  in
  let fn =
    maybe_destruct (module Ext) ~assume_exhaustive ~loc ~modul ~lhs:nested_patterns ~body
  in
  bind_apply ~op_name:Ext.name ~loc ~modul ~arg:nested_boths ~fn
;;

let expand_match (module Ext : Ext) ~extension_kind ~loc ~modul expr cases =
  let expr = maybe_open ~extension_kind ~to_open:(open_on_rhs ~modul) expr in
  Ext.expand_match ~loc ~modul expr cases
;;

let expand_if t ~extension_kind ~loc expr then_ else_ =
  expand_match
    t
    ~extension_kind
    ~loc
    expr
    [ case ~lhs:(pbool ~loc true) ~guard:None ~rhs:then_
    ; case ~lhs:(pbool ~loc false) ~guard:None ~rhs:else_
    ]
;;

let expand_while (module Ext : Ext) ~extension_kind ~loc ~modul ~cond ~body =
  let loop_name = gen_symbol ~prefix:"__let_syntax_loop" () in
  let ploop = pvar ~loc loop_name in
  let eloop = evar ~loc loop_name in
  let loop_call = pexp_apply ~loc eloop [ Nolabel, eunit ~loc ] in
  let loop_body =
    let then_ = bind_apply ~op_name:Ext.name ~loc ~modul ~arg:body ~fn:eloop in
    let else_ = qualified_return ~loc ~modul (eunit ~loc) in
    expand_if (module Ext) ~extension_kind ~modul ~loc cond then_ else_
  in
  let loop_func = pexp_fun ~loc Nolabel None (punit ~loc) loop_body in
  pexp_let ~loc Recursive [ value_binding ~loc ~pat:ploop ~expr:loop_func ] loop_call
;;

module Map : Ext = struct
  let name = "map"

  let disallow_expression _ = function
    | Pexp_while (_, _) -> Error "while%%map is not supported. use while%%bind instead."
    | _ -> Ok ()
  ;;

  let destruct ~assume_exhaustive:_ ~loc:_ ~modul:_ ~lhs:_ ~rhs:_ ~body:_ = None

  let expand_match ~loc ~modul expr cases =
    bind_apply ~loc ~modul ~op_name:name ~arg:expr ~fn:(pexp_function ~loc cases)
  ;;
end

module Bind : Ext = struct
  let name = "bind"

  let disallow_expression { Extension_kind.collapse_binds; _ } = function
    | Pexp_while (_, _) when collapse_binds ->
      Error "while%%bindn is not supported. use while%%bind instead."
    | _ -> Ok ()
  ;;

  let destruct ~assume_exhaustive:_ ~loc:_ ~modul:_ ~lhs:_ ~rhs:_ ~body:_ = None

  let expand_match ~loc ~modul expr cases =
    bind_apply ~loc ~modul ~op_name:name ~arg:expr ~fn:(pexp_function ~loc cases)
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

type pat_exh =
  { pat : pattern
  ; assume_exhaustive : bool
  }

let replace_variable ~f =
  let replacer =
    object
      inherit Ast_traverse.map as super

      method! pattern p =
        let p = super#pattern p in
        let loc = { p.ppat_loc with loc_ghost = true } in
        match p.ppat_desc with
        | Ppat_var v ->
          (match f v with
           | `Rename tmpvar -> ppat_var ~loc { txt = tmpvar; loc = v.loc }
           | `Remove -> ppat_any ~loc)
        | Ppat_alias (sub, v) ->
          (match f v with
           | `Rename tmpvar -> ppat_alias ~loc sub { txt = tmpvar; loc = v.loc }
           | `Remove -> sub)
        | _ -> p
    end
  in
  replacer#pattern
;;

let project_bound_var ~loc ~modul exp ~pat:{ pat; assume_exhaustive } var =
  let project_the_var =
    (* We use a fresh var name because the compiler conflates all definitions with the
       name * location, for the purpose of emitting warnings. *)
    let tmpvar = gen_symbol ~prefix:"__pattern_syntax" () in
    let pattern =
      replace_variable pat ~f:(fun v ->
        if String.equal v.txt var.txt then `Rename tmpvar else `Remove)
    in
    case ~lhs:pattern ~guard:None ~rhs:(evar ~loc tmpvar)
  in
  let catch_all_case = if assume_exhaustive then [] else [ catch_all_case ~loc ] in
  bind_apply
    ~op_name:Map.name
    ~loc
    ~modul
    ~arg:exp
    ~fn:(pexp_function ~loc (project_the_var :: catch_all_case))
;;

let project_bound_vars ~loc ~modul exp ~lhs =
  let loc = { loc with loc_ghost = true } in
  let variables = pattern_variables lhs.pat in
  List.map variables ~f:(fun var ->
    { txt =
        (let expr = project_bound_var ~loc ~modul exp ~pat:lhs var in
         value_binding
           ~loc
           ~pat:(ppat_var ~loc:var.loc var)
           ~expr:(Merlin_helpers.hide_expression expr))
    ; loc
    })
;;

let project_pattern_variables ~assume_exhaustive ~modul vbs =
  List.concat_map vbs ~f:(fun vb ->
    let loc = { vb.pvb_loc with loc_ghost = true } in
    project_bound_vars
      ~loc
      ~modul
      vb.pvb_expr
      ~lhs:{ pat = vb.pvb_pat; assume_exhaustive })
;;

let name_expr expr =
  (* to avoid duplicating non-value expressions *)
  match expr.pexp_desc with
  | Pexp_ident _ -> [], expr
  | _ ->
    let loc = { expr.pexp_loc with loc_ghost = true } in
    let var = gen_symbol ~prefix:"__pattern_syntax" () in
    [ value_binding ~loc ~pat:(pvar ~loc var) ~expr ], evar ~loc var
;;

let warning_attribute ~loc str =
  attribute
    ~loc
    ~name:(Loc.make ~loc "ocaml.warning")
    ~payload:(PStr [ pstr_eval ~loc (estring ~loc str) [] ])
;;

let case_number ~loc ~modul exp indexed_cases =
  { (expand_match
       (module Map)
       ~extension_kind:Extension_kind.default
       ~loc
       ~modul
       exp
       (List.map indexed_cases ~f:(fun (idx, case) ->
          { case with pc_rhs = eint ~loc idx })))
    with
      pexp_attributes = (* Unused variable warnings *)
        [ warning_attribute ~loc "-26-27" ]
  }
;;

let expand_case ~destruct expr (idx, match_case) =
  let loc = { match_case.pc_lhs.ppat_loc with loc_ghost = true } in
  let rhs =
    destruct ~lhs:match_case.pc_lhs ~rhs:expr ~body:match_case.pc_rhs
    |> Option.value ~default:expr
  in
  case ~lhs:(pint ~loc idx) ~guard:None ~rhs
;;

let case_number_cases ~loc ~destruct exp indexed_cases =
  List.map indexed_cases ~f:(expand_case ~destruct exp) @ [ catch_all_case ~loc ]
;;

let indexed_match ~loc ~modul ~destruct ~switch expr cases =
  let expr_binding, expr = name_expr expr in
  let indexed_cases = List.mapi cases ~f:(fun idx case -> idx, case) in
  let case_number = case_number ~loc ~modul expr indexed_cases in
  let assume_exhaustive = List.length cases <= 1 in
  let destruct = destruct ~assume_exhaustive ~loc ~modul in
  let case_number_cases = case_number_cases ~loc ~destruct expr indexed_cases in
  pexp_let
    ~loc
    Nonrecursive
    expr_binding
    (switch ~loc ~modul case_number case_number_cases)
;;

module Sub : Ext = struct
  let name = "sub"

  let disallow_expression _ = function
    (* It is worse to use let%sub...and instead of multiple let%sub in a row, so disallow it. *)
    | Pexp_let (Nonrecursive, _ :: _ :: _, _) ->
      Error "let%sub should not be used with 'and'."
    | Pexp_while (_, _) -> Error "while%sub is not supported"
    | _ -> Ok ()
  ;;

  let sub_return ~loc ~modul ~lhs ~rhs ~body =
    let returned_expression = qualified_return ~loc ~modul rhs in
    bind_apply
      ~op_name:name
      ~loc
      ~modul
      ~arg:returned_expression
      ~fn:(pexp_fun Nolabel None ~loc lhs body)
  ;;

  let destruct ~assume_exhaustive ~loc ~modul ~lhs ~rhs ~body =
    match lhs.ppat_desc with
    | Ppat_var _ -> None
    | _ ->
      let bindings = [ value_binding ~loc ~pat:lhs ~expr:rhs ] in
      let pattern_projections =
        project_pattern_variables ~assume_exhaustive ~modul bindings
      in
      List.fold pattern_projections ~init:body ~f:(fun expr { txt = binding; loc } ->
        sub_return ~loc ~modul ~lhs:binding.pvb_pat ~rhs:binding.pvb_expr ~body:expr)
      |> Option.some
  ;;

  let switch ~loc ~modul case_number case_number_cases =
    Merlin_helpers.hide_expression
      (pexp_apply
         ~loc
         (eoperator ~loc ~modul "switch")
         [ Labelled "match_", case_number
         ; Labelled "branches", eint ~loc (List.length case_number_cases - 1)
         ; Labelled "with_", pexp_function ~loc case_number_cases
         ])
  ;;

  let expand_match ~loc ~modul expr cases =
    let var = gen_symbol ~prefix:"__pattern_syntax" () in
    sub_return
      ~loc
      ~modul
      ~lhs:(pvar ~loc var)
      ~rhs:expr
      ~body:(indexed_match ~loc ~modul ~destruct ~switch (evar ~loc var) cases)
  ;;
end

let expand (module Ext : Ext) extension_kind ?(assume_exhaustive = true) ~modul expr =
  let loc = { expr.pexp_loc with loc_ghost = true } in
  let expansion =
    let expr_desc =
      match Ext.disallow_expression extension_kind expr.pexp_desc with
      | Error message -> Location.raise_errorf ~loc "%s" message
      | Ok () -> expr.pexp_desc
    in
    match expr_desc with
    | Pexp_let (Nonrecursive, bindings, expr) ->
      let bindings =
        List.map bindings ~f:(fun vb ->
          let pvb_pat =
            (* Temporary hack tentatively detecting that the parser
               has expanded `let x : t = e` into `let x : t = (e : t)`.

               For reference, here is the relevant part of the parser:
               https://github.com/ocaml/ocaml/blob/4.07/parsing/parser.mly#L1628 *)
            match vb.pvb_pat.ppat_desc, vb.pvb_expr.pexp_desc with
            | ( Ppat_constraint (p, { ptyp_desc = Ptyp_poly ([], t1); _ })
              , Pexp_constraint (_, t2) )
              when phys_equal t1 t2 || Poly.equal t1 t2 -> p
            | _ -> vb.pvb_pat
          in
          { vb with
            pvb_pat
          ; pvb_expr =
              maybe_open ~extension_kind ~to_open:(open_on_rhs ~modul) vb.pvb_expr
          })
      in
      let f =
        if extension_kind.collapse_binds
        then expand_letn (module Ext) ~assume_exhaustive ~modul
        else expand_let (module Ext) ~assume_exhaustive ~modul
      in
      expand_with_tmp_vars ~loc bindings expr ~f
    | Pexp_let (Recursive, _, _) ->
      let ext_full_name = ext_full_name (module Ext) extension_kind in
      Location.raise_errorf ~loc "'let%%%s' may not be recursive" ext_full_name
    | Pexp_match (expr, cases) ->
      expand_match (module Ext) ~extension_kind ~loc ~modul expr cases
    | Pexp_ifthenelse (expr, then_, else_) ->
      let else_ =
        match else_ with
        | Some else_ -> else_
        | None ->
          let ext_full_name = ext_full_name (module Ext) extension_kind in
          Location.raise_errorf ~loc "'if%%%s' must include an else branch" ext_full_name
      in
      expand_if (module Ext) ~extension_kind ~loc ~modul expr then_ else_
    | Pexp_while (cond, body) ->
      expand_while (module Ext) ~extension_kind ~loc ~modul ~cond ~body
    | _ ->
      Location.raise_errorf
        ~loc
        "'%%%s' can only be used with 'let', 'match', 'while', and 'if'"
        (ext_full_name (module Ext) extension_kind)
  in
  { expansion with pexp_attributes = expr.pexp_attributes @ expansion.pexp_attributes }
;;

let sub = (module Sub : Ext)
let map = (module Map : Ext)
let bind = (module Bind : Ext)
