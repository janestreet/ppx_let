open Core
open Ppxlib

let loc = Location.none
let print_expr expr = Pprintast.string_of_expression expr |> print_string

let expand extension kind ~modul expr =
  List.iteri Locality.all ~f:(fun i locality ->
    if i > 0 then printf "----\n";
    printf !"locality = %{sexp:Locality.t}:\n" locality;
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
    locality = ((allocate_function_on_stack false) (return_value_in_exclave false)):
    Let_syntax.map MY_EXPR ~f:(fun (MY_PAT) -> MY_BODY)
    ----
    locality = ((allocate_function_on_stack true) (return_value_in_exclave false)):
    ((Let_syntax.map MY_EXPR ~f:(local_ fun (MY_PAT) -> MY_BODY))[@nontail ])
    ----
    locality = ((allocate_function_on_stack false) (return_value_in_exclave true)):
    Let_syntax.map MY_EXPR ~f:(fun (MY_PAT) -> exclave_ MY_BODY)
    ----
    locality = ((allocate_function_on_stack true) (return_value_in_exclave true)):
    ((Let_syntax.map MY_EXPR ~f:(local_ fun (MY_PAT) -> exclave_ MY_BODY))
    [@nontail ])
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
    locality = ((allocate_function_on_stack false) (return_value_in_exclave false)):
    X.Let_syntax.Let_syntax.map MY_EXPR ~f:(fun (MY_PAT) -> MY_BODY)
    ----
    locality = ((allocate_function_on_stack true) (return_value_in_exclave false)):
    ((X.Let_syntax.Let_syntax.map MY_EXPR ~f:(local_ fun (MY_PAT) -> MY_BODY))
    [@nontail ])
    ----
    locality = ((allocate_function_on_stack false) (return_value_in_exclave true)):
    X.Let_syntax.Let_syntax.map MY_EXPR ~f:(fun (MY_PAT) -> exclave_ MY_BODY)
    ----
    locality = ((allocate_function_on_stack true) (return_value_in_exclave true)):
    ((X.Let_syntax.Let_syntax.map MY_EXPR
        ~f:(local_ fun (MY_PAT) -> exclave_ MY_BODY))
    [@nontail ])
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
    locality = ((allocate_function_on_stack false) (return_value_in_exclave false)):
    let __let_syntax__009_ = MY_EXPR_1[@@ppxlib.do_not_enter_value ]
    and __let_syntax__010_ = MY_EXPR_2[@@ppxlib.do_not_enter_value ] in
    Let_syntax.map2 __let_syntax__009_ __let_syntax__010_
      ~f:(fun (MY_PAT_1) (MY_PAT_2) -> MY_BODY)
    ----
    locality = ((allocate_function_on_stack true) (return_value_in_exclave false)):
    let __let_syntax__013_ = MY_EXPR_1[@@ppxlib.do_not_enter_value ]
    and __let_syntax__014_ = MY_EXPR_2[@@ppxlib.do_not_enter_value ] in
    ((Let_syntax.map2 __let_syntax__013_ __let_syntax__014_
        ~f:(local_ fun (MY_PAT_1) (MY_PAT_2) -> MY_BODY))
      [@nontail ])
    ----
    locality = ((allocate_function_on_stack false) (return_value_in_exclave true)):
    let __let_syntax__017_ = MY_EXPR_1[@@ppxlib.do_not_enter_value ]
    and __let_syntax__018_ = MY_EXPR_2[@@ppxlib.do_not_enter_value ] in
    Let_syntax.map2 __let_syntax__017_ __let_syntax__018_
      ~f:(fun (MY_PAT_1) (MY_PAT_2) -> exclave_ MY_BODY)
    ----
    locality = ((allocate_function_on_stack true) (return_value_in_exclave true)):
    let __let_syntax__021_ = MY_EXPR_1[@@ppxlib.do_not_enter_value ]
    and __let_syntax__022_ = MY_EXPR_2[@@ppxlib.do_not_enter_value ] in
    ((Let_syntax.map2 __let_syntax__021_ __let_syntax__022_
        ~f:(local_ fun (MY_PAT_1) (MY_PAT_2) -> exclave_ MY_BODY))
      [@nontail ])
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
    locality = ((allocate_function_on_stack false) (return_value_in_exclave false)):
    Let_syntax.map (let open! Let_syntax.Open_on_rhs in MY_EXPR_1)
      ~f:(fun (MY_PAT_1) -> MY_BODY)
    ----
    locality = ((allocate_function_on_stack true) (return_value_in_exclave false)):
    ((Let_syntax.map (let open! Let_syntax.Open_on_rhs in MY_EXPR_1)
        ~f:(local_ fun (MY_PAT_1) -> MY_BODY))
    [@nontail ])
    ----
    locality = ((allocate_function_on_stack false) (return_value_in_exclave true)):
    Let_syntax.map (let open! Let_syntax.Open_on_rhs in MY_EXPR_1)
      ~f:(fun (MY_PAT_1) -> exclave_ MY_BODY)
    ----
    locality = ((allocate_function_on_stack true) (return_value_in_exclave true)):
    ((Let_syntax.map (let open! Let_syntax.Open_on_rhs in MY_EXPR_1)
        ~f:(local_ fun (MY_PAT_1) -> exclave_ MY_BODY))
    [@nontail ])
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
    locality = ((allocate_function_on_stack false) (return_value_in_exclave false)):
    let __let_syntax__029_ = let open! Let_syntax.Open_on_rhs in MY_EXPR_1
    [@@ppxlib.do_not_enter_value ]
    and __let_syntax__030_ = let open! Let_syntax.Open_on_rhs in MY_EXPR_2
    [@@ppxlib.do_not_enter_value ] in
    Let_syntax.map2 __let_syntax__029_ __let_syntax__030_
      ~f:(fun (MY_PAT_1) (MY_PAT_2) -> MY_BODY)
    ----
    locality = ((allocate_function_on_stack true) (return_value_in_exclave false)):
    let __let_syntax__033_ = let open! Let_syntax.Open_on_rhs in MY_EXPR_1
    [@@ppxlib.do_not_enter_value ]
    and __let_syntax__034_ = let open! Let_syntax.Open_on_rhs in MY_EXPR_2
    [@@ppxlib.do_not_enter_value ] in
    ((Let_syntax.map2 __let_syntax__033_ __let_syntax__034_
        ~f:(local_ fun (MY_PAT_1) (MY_PAT_2) -> MY_BODY))
      [@nontail ])
    ----
    locality = ((allocate_function_on_stack false) (return_value_in_exclave true)):
    let __let_syntax__037_ = let open! Let_syntax.Open_on_rhs in MY_EXPR_1
    [@@ppxlib.do_not_enter_value ]
    and __let_syntax__038_ = let open! Let_syntax.Open_on_rhs in MY_EXPR_2
    [@@ppxlib.do_not_enter_value ] in
    Let_syntax.map2 __let_syntax__037_ __let_syntax__038_
      ~f:(fun (MY_PAT_1) (MY_PAT_2) -> exclave_ MY_BODY)
    ----
    locality = ((allocate_function_on_stack true) (return_value_in_exclave true)):
    let __let_syntax__041_ = let open! Let_syntax.Open_on_rhs in MY_EXPR_1
    [@@ppxlib.do_not_enter_value ]
    and __let_syntax__042_ = let open! Let_syntax.Open_on_rhs in MY_EXPR_2
    [@@ppxlib.do_not_enter_value ] in
    ((Let_syntax.map2 __let_syntax__041_ __let_syntax__042_
        ~f:(local_ fun (MY_PAT_1) (MY_PAT_2) -> exclave_ MY_BODY))
      [@nontail ])
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
    locality = ((allocate_function_on_stack false) (return_value_in_exclave false)):
    let __let_syntax__045_ = MY_EXPR_1[@@ppxlib.do_not_enter_value ]
    and __let_syntax__046_ = MY_EXPR_2[@@ppxlib.do_not_enter_value ]
    and __let_syntax__047_ = MY_EXPR_3[@@ppxlib.do_not_enter_value ]
    and __let_syntax__048_ = MY_EXPR_4[@@ppxlib.do_not_enter_value ] in
    Let_syntax.map4 __let_syntax__045_ __let_syntax__046_ __let_syntax__047_
      __let_syntax__048_
      ~f:(fun (MY_PAT_1) (MY_PAT_2) (SUB_PATTERN_1, SUB_PATTERN_2) (MY_PAT_4) ->
            MY_BODY)
    ----
    locality = ((allocate_function_on_stack true) (return_value_in_exclave false)):
    let __let_syntax__053_ = MY_EXPR_1[@@ppxlib.do_not_enter_value ]
    and __let_syntax__054_ = MY_EXPR_2[@@ppxlib.do_not_enter_value ]
    and __let_syntax__055_ = MY_EXPR_3[@@ppxlib.do_not_enter_value ]
    and __let_syntax__056_ = MY_EXPR_4[@@ppxlib.do_not_enter_value ] in
    ((Let_syntax.map4 __let_syntax__053_ __let_syntax__054_ __let_syntax__055_
        __let_syntax__056_
        ~f:(local_ fun (MY_PAT_1) (MY_PAT_2) (SUB_PATTERN_1, SUB_PATTERN_2)
                     (MY_PAT_4) -> MY_BODY))
      [@nontail ])
    ----
    locality = ((allocate_function_on_stack false) (return_value_in_exclave true)):
    let __let_syntax__061_ = MY_EXPR_1[@@ppxlib.do_not_enter_value ]
    and __let_syntax__062_ = MY_EXPR_2[@@ppxlib.do_not_enter_value ]
    and __let_syntax__063_ = MY_EXPR_3[@@ppxlib.do_not_enter_value ]
    and __let_syntax__064_ = MY_EXPR_4[@@ppxlib.do_not_enter_value ] in
    Let_syntax.map4 __let_syntax__061_ __let_syntax__062_ __let_syntax__063_
      __let_syntax__064_
      ~f:(fun (MY_PAT_1) (MY_PAT_2) (SUB_PATTERN_1, SUB_PATTERN_2) (MY_PAT_4) ->
            exclave_ MY_BODY)
    ----
    locality = ((allocate_function_on_stack true) (return_value_in_exclave true)):
    let __let_syntax__069_ = MY_EXPR_1[@@ppxlib.do_not_enter_value ]
    and __let_syntax__070_ = MY_EXPR_2[@@ppxlib.do_not_enter_value ]
    and __let_syntax__071_ = MY_EXPR_3[@@ppxlib.do_not_enter_value ]
    and __let_syntax__072_ = MY_EXPR_4[@@ppxlib.do_not_enter_value ] in
    ((Let_syntax.map4 __let_syntax__069_ __let_syntax__070_ __let_syntax__071_
        __let_syntax__072_
        ~f:(local_ fun (MY_PAT_1) (MY_PAT_2) (SUB_PATTERN_1, SUB_PATTERN_2)
                     (MY_PAT_4) -> exclave_ MY_BODY))
      [@nontail ])
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
    locality = ((allocate_function_on_stack false) (return_value_in_exclave false)):
    let __let_syntax__077_ = MY_EXPR_1[@@ppxlib.do_not_enter_value ]
    and __let_syntax__078_ = MY_EXPR_2[@@ppxlib.do_not_enter_value ]
    and __let_syntax__079_ = MY_EXPR_3[@@ppxlib.do_not_enter_value ]
    and __let_syntax__080_ = MY_EXPR_4[@@ppxlib.do_not_enter_value ] in
    Let_syntax.bind4 __let_syntax__077_ __let_syntax__078_ __let_syntax__079_
      __let_syntax__080_
      ~f:(fun (MY_PAT_1) (MY_PAT_2) (SUB_PATTERN_1, SUB_PATTERN_2) (MY_PAT_4) ->
            MY_BODY)
    ----
    locality = ((allocate_function_on_stack true) (return_value_in_exclave false)):
    let __let_syntax__085_ = MY_EXPR_1[@@ppxlib.do_not_enter_value ]
    and __let_syntax__086_ = MY_EXPR_2[@@ppxlib.do_not_enter_value ]
    and __let_syntax__087_ = MY_EXPR_3[@@ppxlib.do_not_enter_value ]
    and __let_syntax__088_ = MY_EXPR_4[@@ppxlib.do_not_enter_value ] in
    ((Let_syntax.bind4 __let_syntax__085_ __let_syntax__086_ __let_syntax__087_
        __let_syntax__088_
        ~f:(local_ fun (MY_PAT_1) (MY_PAT_2) (SUB_PATTERN_1, SUB_PATTERN_2)
                     (MY_PAT_4) -> MY_BODY))
      [@nontail ])
    ----
    locality = ((allocate_function_on_stack false) (return_value_in_exclave true)):
    let __let_syntax__093_ = MY_EXPR_1[@@ppxlib.do_not_enter_value ]
    and __let_syntax__094_ = MY_EXPR_2[@@ppxlib.do_not_enter_value ]
    and __let_syntax__095_ = MY_EXPR_3[@@ppxlib.do_not_enter_value ]
    and __let_syntax__096_ = MY_EXPR_4[@@ppxlib.do_not_enter_value ] in
    Let_syntax.bind4 __let_syntax__093_ __let_syntax__094_ __let_syntax__095_
      __let_syntax__096_
      ~f:(fun (MY_PAT_1) (MY_PAT_2) (SUB_PATTERN_1, SUB_PATTERN_2) (MY_PAT_4) ->
            exclave_ MY_BODY)
    ----
    locality = ((allocate_function_on_stack true) (return_value_in_exclave true)):
    let __let_syntax__101_ = MY_EXPR_1[@@ppxlib.do_not_enter_value ]
    and __let_syntax__102_ = MY_EXPR_2[@@ppxlib.do_not_enter_value ]
    and __let_syntax__103_ = MY_EXPR_3[@@ppxlib.do_not_enter_value ]
    and __let_syntax__104_ = MY_EXPR_4[@@ppxlib.do_not_enter_value ] in
    ((Let_syntax.bind4 __let_syntax__101_ __let_syntax__102_ __let_syntax__103_
        __let_syntax__104_
        ~f:(local_ fun (MY_PAT_1) (MY_PAT_2) (SUB_PATTERN_1, SUB_PATTERN_2)
                     (MY_PAT_4) -> exclave_ MY_BODY))
      [@nontail ])
    |}]
;;
