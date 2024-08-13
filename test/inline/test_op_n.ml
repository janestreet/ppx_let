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
    Let_syntax.map MY_EXPR ~f:(fun (MY_PAT) -> exclave_ MY_BODY)
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
    X.Let_syntax.Let_syntax.map MY_EXPR ~f:(fun (MY_PAT) -> exclave_ MY_BODY)
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
    let __let_syntax__005_ = MY_EXPR_1[@@ppxlib.do_not_enter_value ]
    and __let_syntax__006_ = MY_EXPR_2[@@ppxlib.do_not_enter_value ] in
    Let_syntax.map2 __let_syntax__005_ __let_syntax__006_
      ~f:(fun (MY_PAT_1) (MY_PAT_2) -> MY_BODY)
    ----
    locality = local:
    let __let_syntax__009_ = MY_EXPR_1[@@ppxlib.do_not_enter_value ]
    and __let_syntax__010_ = MY_EXPR_2[@@ppxlib.do_not_enter_value ] in
    Let_syntax.map2 __let_syntax__009_ __let_syntax__010_
      ~f:(fun (MY_PAT_1) (MY_PAT_2) -> exclave_ MY_BODY)
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
      ~f:(fun (MY_PAT_1) -> exclave_ MY_BODY)
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
    let __let_syntax__015_ = let open! Let_syntax.Open_on_rhs in MY_EXPR_1
    [@@ppxlib.do_not_enter_value ]
    and __let_syntax__016_ = let open! Let_syntax.Open_on_rhs in MY_EXPR_2
    [@@ppxlib.do_not_enter_value ] in
    Let_syntax.map2 __let_syntax__015_ __let_syntax__016_
      ~f:(fun (MY_PAT_1) (MY_PAT_2) -> MY_BODY)
    ----
    locality = local:
    let __let_syntax__019_ = let open! Let_syntax.Open_on_rhs in MY_EXPR_1
    [@@ppxlib.do_not_enter_value ]
    and __let_syntax__020_ = let open! Let_syntax.Open_on_rhs in MY_EXPR_2
    [@@ppxlib.do_not_enter_value ] in
    Let_syntax.map2 __let_syntax__019_ __let_syntax__020_
      ~f:(fun (MY_PAT_1) (MY_PAT_2) -> exclave_ MY_BODY)
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
    let __let_syntax__023_ = MY_EXPR_1[@@ppxlib.do_not_enter_value ]
    and __let_syntax__024_ = MY_EXPR_2[@@ppxlib.do_not_enter_value ]
    and __let_syntax__025_ = MY_EXPR_3[@@ppxlib.do_not_enter_value ]
    and __let_syntax__026_ = MY_EXPR_4[@@ppxlib.do_not_enter_value ] in
    Let_syntax.map4 __let_syntax__023_ __let_syntax__024_ __let_syntax__025_
      __let_syntax__026_
      ~f:(fun (MY_PAT_1) (MY_PAT_2) (SUB_PATTERN_1, SUB_PATTERN_2) (MY_PAT_4) ->
            MY_BODY)
    ----
    locality = local:
    let __let_syntax__031_ = MY_EXPR_1[@@ppxlib.do_not_enter_value ]
    and __let_syntax__032_ = MY_EXPR_2[@@ppxlib.do_not_enter_value ]
    and __let_syntax__033_ = MY_EXPR_3[@@ppxlib.do_not_enter_value ]
    and __let_syntax__034_ = MY_EXPR_4[@@ppxlib.do_not_enter_value ] in
    Let_syntax.map4 __let_syntax__031_ __let_syntax__032_ __let_syntax__033_
      __let_syntax__034_
      ~f:(fun (MY_PAT_1) (MY_PAT_2) (SUB_PATTERN_1, SUB_PATTERN_2) (MY_PAT_4) ->
            exclave_ MY_BODY)
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
    let __let_syntax__039_ = MY_EXPR_1[@@ppxlib.do_not_enter_value ]
    and __let_syntax__040_ = MY_EXPR_2[@@ppxlib.do_not_enter_value ]
    and __let_syntax__041_ = MY_EXPR_3[@@ppxlib.do_not_enter_value ]
    and __let_syntax__042_ = MY_EXPR_4[@@ppxlib.do_not_enter_value ] in
    Let_syntax.bind4 __let_syntax__039_ __let_syntax__040_ __let_syntax__041_
      __let_syntax__042_
      ~f:(fun (MY_PAT_1) (MY_PAT_2) (SUB_PATTERN_1, SUB_PATTERN_2) (MY_PAT_4) ->
            MY_BODY)
    ----
    locality = local:
    let __let_syntax__047_ = MY_EXPR_1[@@ppxlib.do_not_enter_value ]
    and __let_syntax__048_ = MY_EXPR_2[@@ppxlib.do_not_enter_value ]
    and __let_syntax__049_ = MY_EXPR_3[@@ppxlib.do_not_enter_value ]
    and __let_syntax__050_ = MY_EXPR_4[@@ppxlib.do_not_enter_value ] in
    Let_syntax.bind4 __let_syntax__047_ __let_syntax__048_ __let_syntax__049_
      __let_syntax__050_
      ~f:(fun (MY_PAT_1) (MY_PAT_2) (SUB_PATTERN_1, SUB_PATTERN_2) (MY_PAT_4) ->
            exclave_ MY_BODY)
    |}]
;;
