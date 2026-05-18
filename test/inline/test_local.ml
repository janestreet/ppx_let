open Core
open Ppxlib

let loc = Location.none
let print_expr expr = Pprintast.string_of_expression expr |> print_string

(* Doesn't actually check anything except within JS walls -- the public compiler doesn't
   yet support local allocations. *)

let assert_zero_alloc f = exclave_
  let allocations_before = Gc.major_plus_minor_words () in
  let r = f () in
  let allocations_after = Gc.major_plus_minor_words () in
  let allocations = allocations_after - allocations_before in
  [%test_result: int] allocations ~expect:0;
  r
;;

let check_some (local_ n) =
  match n with
  | Some () -> ()
  | None -> failwith "must be some"
;;

let%expect_test "while%bindl trivial test" =
  let i = ref 0 in
  let (r : unit option) =
    assert_zero_alloc (fun () -> exclave_
      while%bindl.Local_option
        incr i;
        Some (!i <= 5)
      do
        Some ()
      done)
  in
  check_some r [@nontail]
;;

let%expect_test "while%bindl_val trivial test" =
  let i = ref 0 in
  let (r : unit option) =
    assert_zero_alloc (fun () -> exclave_
      while%bindl_val.Local_option
        incr i;
        Some (!i <= 5)
      do
        Some ()
      done)
  in
  check_some r [@nontail]
;;

let%expect_test "while%bindl_fun trivial test" =
  let i = ref 0 in
  let (r : unit Or_null.t) =
    assert_zero_alloc (fun () -> exclave_
      while%bindl_fun.Or_null
        incr i;
        This (!i <= 5)
      do
        This ()
      done)
  in
  r |> Or_null.to_option |> check_some
;;

let%expect_test "for%bindl trivial test" =
  let (r : unit option) =
    assert_zero_alloc (fun () -> exclave_
      for%bindl.Local_option _ = 1 to 5 do
        Some ()
      done)
  in
  check_some r [@nontail]
;;

let%expect_test "for%bindl downto trivial test" =
  let (r : unit option) =
    assert_zero_alloc (fun () -> exclave_
      for%bindl.Local_option _ = 5 downto 1 do
        Some ()
      done)
  in
  check_some r [@nontail]
;;

let%expect_test "for%bindl_val trivial test" =
  let (r : unit option) =
    assert_zero_alloc (fun () -> exclave_
      for%bindl_val.Local_option _ = 1 to 5 do
        Some ()
      done)
  in
  check_some r [@nontail]
;;

let%expect_test "for%bindl_fun trivial test" =
  let (r : unit Or_null.t) =
    assert_zero_alloc (fun () -> exclave_
      for%bindl_fun.Or_null _ = 1 to 5 do
        This ()
      done)
  in
  r |> Or_null.to_option |> check_some
;;

let local : Locality.t =
  { allocate_function_on_stack = true; return_value_in_exclave = true }
;;

let%expect_test "for%bindl expansion" =
  Ppx_let_expander.expand
    Ppx_let_expander.bind
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality:local
    [%expr
      for i = START to STOP do
        BODY
      done]
  |> print_expr;
  [%expect
    {|
    let i = START[@@ppxlib.do_not_enter_value ]
    and __let_syntax_for_loop_bound__001_ = STOP[@@ppxlib.do_not_enter_value ] in
    if i <= __let_syntax_for_loop_bound__001_
    then
      let rec __let_syntax_loop__002_ =
        (fun i ->
           exclave_ ((Let_syntax.bind BODY
                        ~f:(fun () ->
                              exclave_ if i < __let_syntax_for_loop_bound__001_
                                       then
                                         let i = i + 1[@@ppxlib.do_not_enter_value
                                                        ] in
                                         __let_syntax_loop__002_ i
                                       else Let_syntax.return () : @ local))
             [@nontail ]) : @ local)[@@ppxlib.do_not_enter_value ] in
      ((__let_syntax_loop__002_ i)[@nontail ])
    else Let_syntax.return ()
    |}]
;;

let%expect_test "for%bindl_val expansion" =
  Ppx_let_expander.expand
    Ppx_let_expander.bind
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality:{ local with allocate_function_on_stack = false }
    [%expr
      for i = START to STOP do
        BODY
      done]
  |> print_expr;
  [%expect
    {|
    let i = START[@@ppxlib.do_not_enter_value ]
    and __let_syntax_for_loop_bound__003_ = STOP[@@ppxlib.do_not_enter_value ] in
    if i <= __let_syntax_for_loop_bound__003_
    then
      let rec __let_syntax_loop__004_ i =
        exclave_ Let_syntax.bind BODY
                   ~f:(fun () ->
                         exclave_ if i < __let_syntax_for_loop_bound__003_
                                  then
                                    let i = i + 1[@@ppxlib.do_not_enter_value ] in
                                    __let_syntax_loop__004_ i
                                  else Let_syntax.return ())[@@ppxlib.do_not_enter_value
                                                              ] in
      __let_syntax_loop__004_ i
    else Let_syntax.return ()
    |}]
;;

let%expect_test "for%bindl_fun expansion" =
  Ppx_let_expander.expand
    Ppx_let_expander.bind
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality:{ local with return_value_in_exclave = false }
    [%expr
      for i = START to STOP do
        BODY
      done]
  |> print_expr;
  [%expect
    {|
    let i = START[@@ppxlib.do_not_enter_value ]
    and __let_syntax_for_loop_bound__005_ = STOP[@@ppxlib.do_not_enter_value ] in
    if i <= __let_syntax_for_loop_bound__005_
    then
      let rec __let_syntax_loop__006_ =
        (fun i ->
           ((Let_syntax.bind BODY
               ~f:(fun () ->
                     if i < __let_syntax_for_loop_bound__005_
                     then
                       let i = i + 1[@@ppxlib.do_not_enter_value ] in
                       __let_syntax_loop__006_ i
                     else Let_syntax.return () : @ local))
           [@nontail ]) : @ local)[@@ppxlib.do_not_enter_value ] in
      ((__let_syntax_loop__006_ i)[@nontail ])
    else Let_syntax.return ()
    |}]
;;

let%expect_test "while%bindl expansion" =
  Ppx_let_expander.expand
    Ppx_let_expander.bind
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality:local
    [%expr
      while COND do
        BODY
      done]
  |> print_expr;
  [%expect
    {|
    ((Let_syntax.bind COND
        ~f:(fun __let_syntax__009_ ->
              exclave_ match __let_syntax__009_ with
                       | true ->
                           let rec __let_syntax_loop__007_ =
                             (fun () ->
                                exclave_ ((Let_syntax.bind BODY
                                             ~f:(fun () ->
                                                   exclave_ ((Let_syntax.bind
                                                                COND
                                                                ~f:(fun
                                                                      __let_syntax__008_
                                                                      ->
                                                                      exclave_
                                                                        match __let_syntax__008_
                                                                        with
                                                                        |
                                                                        true ->
                                                                        __let_syntax_loop__007_
                                                                        ()
                                                                        |
                                                                        false ->
                                                                        Let_syntax.return
                                                                        ()))
                                                     [@nontail ]) : @ local))
                                  [@nontail ]) : @ local)[@@ppxlib.do_not_enter_value
                                                           ] in
                           ((__let_syntax_loop__007_ ())[@nontail ])
                       | false -> Let_syntax.return ()))
    [@nontail ])
    |}]
;;

let%expect_test "while%bindl_val expansion" =
  Ppx_let_expander.expand
    Ppx_let_expander.bind
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality:{ local with allocate_function_on_stack = false }
    [%expr
      while COND do
        BODY
      done]
  |> print_expr;
  [%expect
    {|
    Let_syntax.bind COND
      ~f:(fun __let_syntax__012_ ->
            exclave_ match __let_syntax__012_ with
                     | true ->
                         let rec __let_syntax_loop__010_ () =
                           exclave_ Let_syntax.bind BODY
                                      ~f:(fun () ->
                                            exclave_ Let_syntax.bind COND
                                                       ~f:(fun __let_syntax__011_
                                                             ->
                                                             exclave_ match __let_syntax__011_
                                                                      with
                                                                      | true ->
                                                                        __let_syntax_loop__010_
                                                                        ()
                                                                      | false ->
                                                                        Let_syntax.return
                                                                        ()))
                           [@@ppxlib.do_not_enter_value ] in
                         __let_syntax_loop__010_ ()
                     | false -> Let_syntax.return ())
    |}]
;;

let%expect_test "while%bindl_fun expansion" =
  Ppx_let_expander.expand
    Ppx_let_expander.bind
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality:{ local with return_value_in_exclave = false }
    [%expr
      while COND do
        BODY
      done]
  |> print_expr;
  [%expect
    {|
    ((Let_syntax.bind COND
        ~f:(function
            | true ->
                let rec __let_syntax_loop__013_ =
                  (fun () ->
                     ((Let_syntax.bind BODY
                         ~f:(fun () ->
                               ((Let_syntax.bind COND
                                   ~f:(function
                                       | true -> __let_syntax_loop__013_ ()
                                       | false -> Let_syntax.return ()))
                               [@nontail ]) : @ local))
                     [@nontail ]) : @ local)[@@ppxlib.do_not_enter_value ] in
                ((__let_syntax_loop__013_ ())[@nontail ])
            | false -> Let_syntax.return ()))
    [@nontail ])
    |}]
;;

let%expect_test "let%bindl expansion" =
  Ppx_let_expander.expand
    Ppx_let_expander.bind
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality:local
    [%expr
      let PATTERN1 = return EXPRESSION1
      and PATTERN2 = return EXPRESSION2 in
      return EXPRESSION3]
  |> print_expr;
  [%expect
    {|
    let __let_syntax__014_ = return EXPRESSION1[@@ppxlib.do_not_enter_value ]
    and __let_syntax__015_ = return EXPRESSION2[@@ppxlib.do_not_enter_value ] in
    ((Let_syntax.bind (Let_syntax.both __let_syntax__014_ __let_syntax__015_)
        ~f:(fun (PATTERN1, PATTERN2) ->
              exclave_ let () = ()[@@merlin.hide ] in return EXPRESSION3 : @ local))
      [@nontail ])
    |}]
;;

let%expect_test "let%bindl_val expansion" =
  Ppx_let_expander.expand
    Ppx_let_expander.bind
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality:{ local with allocate_function_on_stack = false }
    [%expr
      let PATTERN1 = return EXPRESSION1
      and PATTERN2 = return EXPRESSION2 in
      return EXPRESSION3]
  |> print_expr;
  [%expect
    {|
    let __let_syntax__017_ = return EXPRESSION1[@@ppxlib.do_not_enter_value ]
    and __let_syntax__018_ = return EXPRESSION2[@@ppxlib.do_not_enter_value ] in
    Let_syntax.bind (Let_syntax.both __let_syntax__017_ __let_syntax__018_)
      ~f:(fun (PATTERN1, PATTERN2) ->
            exclave_ let () = ()[@@merlin.hide ] in return EXPRESSION3)
    |}]
;;

let%expect_test "let%bindl_fun expansion" =
  Ppx_let_expander.expand
    Ppx_let_expander.bind
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality:{ local with return_value_in_exclave = false }
    [%expr
      let PATTERN1 = return EXPRESSION1
      and PATTERN2 = return EXPRESSION2 in
      return EXPRESSION3]
  |> print_expr;
  [%expect
    {|
    let __let_syntax__020_ = return EXPRESSION1[@@ppxlib.do_not_enter_value ]
    and __let_syntax__021_ = return EXPRESSION2[@@ppxlib.do_not_enter_value ] in
    ((Let_syntax.bind (Let_syntax.both __let_syntax__020_ __let_syntax__021_)
        ~f:(fun (PATTERN1, PATTERN2) ->
              let () = ()[@@merlin.hide ] in return EXPRESSION3 : @ local))
      [@nontail ])
    |}]
;;

let%expect_test "let%mapl expansion" =
  Ppx_let_expander.expand
    Ppx_let_expander.map
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality:local
    [%expr
      let PATTERN1 = return EXPRESSION1
      and PATTERN2 = return EXPRESSION2 in
      return EXPRESSION3]
  |> print_expr;
  [%expect
    {|
    let __let_syntax__023_ = return EXPRESSION1[@@ppxlib.do_not_enter_value ]
    and __let_syntax__024_ = return EXPRESSION2[@@ppxlib.do_not_enter_value ] in
    ((Let_syntax.map (Let_syntax.both __let_syntax__023_ __let_syntax__024_)
        ~f:(fun (PATTERN1, PATTERN2) ->
              exclave_ let () = ()[@@merlin.hide ] in return EXPRESSION3 : @ local))
      [@nontail ])
    |}]
;;

let something_to_tail_call () = exclave_ Some ()

let%expect_test "make sure let%bindl and let%mapl work well together" =
  let r : unit option option option =
    assert_zero_alloc (fun () -> exclave_
      match%bindl.Local_option Some () with
      | () ->
        let open Local_option.Let_syntax in
        let%bindl () = Some ()
        and () = Some () in
        let%mapl () = Some ()
        and () = Some () in
        let%bindl () = Some () in
        let%mapl () = Some () in
        something_to_tail_call () [@nontail])
  in
  match r with
  | Some (Some x) -> check_some x [@nontail]
  | _ -> failwith "how?"
;;

let%expect_test "bind4" =
  let r : unit option =
    assert_zero_alloc (fun () -> exclave_
      let%bindnl.Local_option () = Some ()
      and () = Some ()
      and () = Some ()
      and () = Some () in
      Some ())
  in
  check_some r [@nontail]
;;

let%expect_test "map4" =
  let r : unit option =
    assert_zero_alloc (fun () -> exclave_
      let%mapnl.Local_option () = Some ()
      and () = Some ()
      and () = Some ()
      and () = Some () in
      ())
  in
  check_some r [@nontail]
;;

let%expect_test "match%bindl" =
  let r : unit option =
    assert_zero_alloc (fun () -> exclave_
      match%bindl.Local_option Some `hello with
      | `hello -> Some ())
  in
  check_some r [@nontail]
;;

let%expect_test "if%bindl" =
  let r : unit option =
    assert_zero_alloc (fun () -> exclave_
      if%bindl.Local_option Some true then Some () else failwith "impossible")
  in
  check_some r [@nontail]
;;

let%expect_test "if%mapl" =
  let r : unit option =
    assert_zero_alloc (fun () -> exclave_
      if%mapl.Local_option Some true then () else failwith "impossible")
  in
  check_some r [@nontail]
;;
