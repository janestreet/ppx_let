open Core
open Ppxlib

let loc = Location.none
let print_expr expr = Pprintast.string_of_expression expr |> print_string

(* Doesn't actually check anything except within JS walls -- the public compiler doesn't
   yet support local allocations. *)

let assert_zero_alloc f =
  let allocations_before = Gc.major_plus_minor_words () in
  let r = f () in
  let allocations_after = Gc.major_plus_minor_words () in
  let allocations = allocations_after - allocations_before in
  [%test_result: int] allocations ~expect:0;
  r
;;

let check_some n =
  match n with
  | Some () -> ()
  | None -> failwith "must be some"
;;

let%expect_test "while%bindl trivial test" =
  let i = ref 0 in
  let (r : unit option) =
    assert_zero_alloc (fun () ->
      while%bindl.Local_option
        incr i;
        Some (!i <= 5)
      do
        Some ()
      done)
  in
  check_some r [@nontail]
;;

let%expect_test "let%bindl expansion" =
  Ppx_let_expander.expand
    Ppx_let_expander.bind
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality:`local
    [%expr
      let PATTERN1 = return EXPRESSION1
      and PATTERN2 = return EXPRESSION2 in
      return EXPRESSION3]
  |> print_expr;
  [%expect
    {|
    let __let_syntax__001_ = return EXPRESSION1[@@ppxlib.do_not_enter_value ]
    and __let_syntax__002_ = return EXPRESSION2[@@ppxlib.do_not_enter_value ] in
    Let_syntax.bind (Let_syntax.both __let_syntax__001_ __let_syntax__002_)
      ~f:(fun (PATTERN1, PATTERN2) ->
            local_ let __nontail__004_ = return EXPRESSION3 in __nontail__004_)
    |}]
;;

let%expect_test "let%mapl expansion" =
  Ppx_let_expander.expand
    Ppx_let_expander.map
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    ~locality:`local
    [%expr
      let PATTERN1 = return EXPRESSION1
      and PATTERN2 = return EXPRESSION2 in
      return EXPRESSION3]
  |> print_expr;
  [%expect
    {|
    let __let_syntax__005_ = return EXPRESSION1[@@ppxlib.do_not_enter_value ]
    and __let_syntax__006_ = return EXPRESSION2[@@ppxlib.do_not_enter_value ] in
    Let_syntax.map (Let_syntax.both __let_syntax__005_ __let_syntax__006_)
      ~f:(fun (PATTERN1, PATTERN2) ->
            local_ let __nontail__008_ = return EXPRESSION3 in __nontail__008_)
    |}]
;;

let something_to_tail_call () = Some ()

let%expect_test "make sure let%bindl and let%mapl work well together" =
  let r : unit option option option =
    assert_zero_alloc (fun () ->
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
    assert_zero_alloc (fun () ->
      let%bindln.Local_option () = Some ()
      and () = Some ()
      and () = Some ()
      and () = Some () in
      Some ())
  in
  check_some r [@nontail]
;;

let%expect_test "map4" =
  let r : unit option =
    assert_zero_alloc (fun () ->
      let%mapln.Local_option () = Some ()
      and () = Some ()
      and () = Some ()
      and () = Some () in
      ())
  in
  check_some r [@nontail]
;;

let%expect_test "match%bindl" =
  let r : unit option =
    assert_zero_alloc (fun () ->
      match%bindl.Local_option Some `hello with
      | `hello -> Some ())
  in
  check_some r [@nontail]
;;

let%expect_test "if%bindl" =
  let r : unit option =
    assert_zero_alloc (fun () ->
      if%bindl.Local_option Some true then Some () else failwith "impossible")
  in
  check_some r [@nontail]
;;

let%expect_test "if%mapl" =
  let r : unit option =
    assert_zero_alloc (fun () ->
      if%mapl.Local_option Some true then () else failwith "impossible")
  in
  check_some r [@nontail]
;;
