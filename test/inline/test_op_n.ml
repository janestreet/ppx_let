open Core
open Ppxlib

let all_localities = [ `global; `local ]
let loc = Location.none
let print_expr expr = Pprintast.string_of_expression expr |> print_string

let expand extension kind ~modul expr =
  List.iteri all_localities ~f:(fun i locality ->
    if i > 0 then printf "----\n";
    printf !"locality = %{sexp:[`local|`global]}:\n" locality;
    Ppx_let_expander.expand extension kind ~modul ~locality expr |> print_expr;
    printf "\n")
;;

let%expect_test "single pattern map" =
  expand
    Ppx_let_expander.map
    Ppx_let_expander.Extension_kind.n
    ~modul:None
    [%expr
      let MY_PAT = MY_EXPR in
      MY_BODY];
  [%expect
    {|
    locality = global:
    Let_syntax.map MY_EXPR ~f:(fun (MY_PAT) -> MY_BODY)
    ----
    locality = local:
    Let_syntax.map MY_EXPR
      ~f:(fun (MY_PAT) -> local_ let __nontail__002_ = MY_BODY in __nontail__002_)
    |}]
;;

let%expect_test "single pattern map with modul" =
  expand
    Ppx_let_expander.map
    Ppx_let_expander.Extension_kind.n
    ~modul:(Some { txt = Longident.Lident "X"; loc = Location.none })
    [%expr
      let MY_PAT = MY_EXPR in
      MY_BODY];
  [%expect
    {|
    locality = global:
    X.Let_syntax.Let_syntax.map MY_EXPR ~f:(fun (MY_PAT) -> MY_BODY)
    ----
    locality = local:
    X.Let_syntax.Let_syntax.map MY_EXPR
      ~f:(fun (MY_PAT) -> local_ let __nontail__005_ = MY_BODY in __nontail__005_)
    |}]
;;

let%expect_test "double pattern map" =
  expand
    Ppx_let_expander.map
    Ppx_let_expander.Extension_kind.n
    ~modul:None
    [%expr
      let MY_PAT_1 = MY_EXPR_1
      and MY_PAT_2 = MY_EXPR_2 in
      MY_BODY];
  [%expect
    {|
    locality = global:
    let __let_syntax__007_ = MY_EXPR_1[@@ppxlib.do_not_enter_value ]
    and __let_syntax__008_ = MY_EXPR_2[@@ppxlib.do_not_enter_value ] in
    Let_syntax.map2 __let_syntax__007_ __let_syntax__008_
      ~f:(fun (MY_PAT_1) (MY_PAT_2) -> MY_BODY)
    ----
    locality = local:
    let __let_syntax__011_ = MY_EXPR_1[@@ppxlib.do_not_enter_value ]
    and __let_syntax__012_ = MY_EXPR_2[@@ppxlib.do_not_enter_value ] in
    Let_syntax.map2 __let_syntax__011_ __let_syntax__012_
      ~f:(fun (MY_PAT_1) (MY_PAT_2) ->
            local_ let __nontail__013_ = MY_BODY in __nontail__013_)
    |}]
;;

let%expect_test "single pattern map open" =
  expand
    Ppx_let_expander.map
    Ppx_let_expander.Extension_kind.n_open
    ~modul:None
    [%expr
      let MY_PAT_1 = MY_EXPR_1 in
      MY_BODY];
  [%expect
    {|
    locality = global:
    Let_syntax.map (let open! Let_syntax.Open_on_rhs in MY_EXPR_1)
      ~f:(fun (MY_PAT_1) -> MY_BODY)
    ----
    locality = local:
    Let_syntax.map (let open! Let_syntax.Open_on_rhs in MY_EXPR_1)
      ~f:(fun (MY_PAT_1) ->
            local_ let __nontail__017_ = MY_BODY in __nontail__017_)
    |}]
;;

let%expect_test "double pattern map open" =
  expand
    Ppx_let_expander.map
    Ppx_let_expander.Extension_kind.n_open
    ~modul:None
    [%expr
      let MY_PAT_1 = MY_EXPR_1
      and MY_PAT_2 = MY_EXPR_2 in
      MY_BODY];
  [%expect
    {|
    locality = global:
    let __let_syntax__019_ = let open! Let_syntax.Open_on_rhs in MY_EXPR_1
    [@@ppxlib.do_not_enter_value ]
    and __let_syntax__020_ = let open! Let_syntax.Open_on_rhs in MY_EXPR_2
    [@@ppxlib.do_not_enter_value ] in
    Let_syntax.map2 __let_syntax__019_ __let_syntax__020_
      ~f:(fun (MY_PAT_1) (MY_PAT_2) -> MY_BODY)
    ----
    locality = local:
    let __let_syntax__023_ = let open! Let_syntax.Open_on_rhs in MY_EXPR_1
    [@@ppxlib.do_not_enter_value ]
    and __let_syntax__024_ = let open! Let_syntax.Open_on_rhs in MY_EXPR_2
    [@@ppxlib.do_not_enter_value ] in
    Let_syntax.map2 __let_syntax__023_ __let_syntax__024_
      ~f:(fun (MY_PAT_1) (MY_PAT_2) ->
            local_ let __nontail__025_ = MY_BODY in __nontail__025_)
    |}]
;;

let%expect_test "quadruple pattern map" =
  expand
    Ppx_let_expander.map
    Ppx_let_expander.Extension_kind.n
    ~modul:None
    [%expr
      let MY_PAT_1 = MY_EXPR_1
      and MY_PAT_2 = MY_EXPR_2
      and SUB_PATTERN_1, SUB_PATTERN_2 = MY_EXPR_3
      and MY_PAT_4 = MY_EXPR_4 in
      MY_BODY];
  [%expect
    {|
    locality = global:
    let __let_syntax__028_ = MY_EXPR_1[@@ppxlib.do_not_enter_value ]
    and __let_syntax__029_ = MY_EXPR_2[@@ppxlib.do_not_enter_value ]
    and __let_syntax__030_ = MY_EXPR_3[@@ppxlib.do_not_enter_value ]
    and __let_syntax__031_ = MY_EXPR_4[@@ppxlib.do_not_enter_value ] in
    Let_syntax.map4 __let_syntax__028_ __let_syntax__029_ __let_syntax__030_
      __let_syntax__031_
      ~f:(fun (MY_PAT_1) (MY_PAT_2) (SUB_PATTERN_1, SUB_PATTERN_2) (MY_PAT_4) ->
            MY_BODY)
    ----
    locality = local:
    let __let_syntax__036_ = MY_EXPR_1[@@ppxlib.do_not_enter_value ]
    and __let_syntax__037_ = MY_EXPR_2[@@ppxlib.do_not_enter_value ]
    and __let_syntax__038_ = MY_EXPR_3[@@ppxlib.do_not_enter_value ]
    and __let_syntax__039_ = MY_EXPR_4[@@ppxlib.do_not_enter_value ] in
    Let_syntax.map4 __let_syntax__036_ __let_syntax__037_ __let_syntax__038_
      __let_syntax__039_
      ~f:(fun (MY_PAT_1) (MY_PAT_2) (SUB_PATTERN_1, SUB_PATTERN_2) (MY_PAT_4) ->
            local_ let __nontail__040_ = MY_BODY in __nontail__040_)
    |}]
;;

let%expect_test "quadruple pattern bind" =
  expand
    Ppx_let_expander.bind
    Ppx_let_expander.Extension_kind.n
    ~modul:None
    [%expr
      let MY_PAT_1 = MY_EXPR_1
      and MY_PAT_2 = MY_EXPR_2
      and SUB_PATTERN_1, SUB_PATTERN_2 = MY_EXPR_3
      and MY_PAT_4 = MY_EXPR_4 in
      MY_BODY];
  [%expect
    {|
    locality = global:
    let __let_syntax__045_ = MY_EXPR_1[@@ppxlib.do_not_enter_value ]
    and __let_syntax__046_ = MY_EXPR_2[@@ppxlib.do_not_enter_value ]
    and __let_syntax__047_ = MY_EXPR_3[@@ppxlib.do_not_enter_value ]
    and __let_syntax__048_ = MY_EXPR_4[@@ppxlib.do_not_enter_value ] in
    Let_syntax.bind4 __let_syntax__045_ __let_syntax__046_ __let_syntax__047_
      __let_syntax__048_
      ~f:(fun (MY_PAT_1) (MY_PAT_2) (SUB_PATTERN_1, SUB_PATTERN_2) (MY_PAT_4) ->
            MY_BODY)
    ----
    locality = local:
    let __let_syntax__053_ = MY_EXPR_1[@@ppxlib.do_not_enter_value ]
    and __let_syntax__054_ = MY_EXPR_2[@@ppxlib.do_not_enter_value ]
    and __let_syntax__055_ = MY_EXPR_3[@@ppxlib.do_not_enter_value ]
    and __let_syntax__056_ = MY_EXPR_4[@@ppxlib.do_not_enter_value ] in
    Let_syntax.bind4 __let_syntax__053_ __let_syntax__054_ __let_syntax__055_
      __let_syntax__056_
      ~f:(fun (MY_PAT_1) (MY_PAT_2) (SUB_PATTERN_1, SUB_PATTERN_2) (MY_PAT_4) ->
            local_ let __nontail__057_ = MY_BODY in __nontail__057_)
    |}]
;;
