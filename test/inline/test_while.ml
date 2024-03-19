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

let%expect_test "while%bind expansion" =
  expand
    Ppx_let_expander.bind
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    [%expr
      while MY_CONDITION do
        MY_BODY
      done];
  [%expect
    {|
    locality = global:
    let rec __let_syntax_loop__001_ () =
      Let_syntax.bind MY_CONDITION
        ~f:(function
            | true -> Let_syntax.bind MY_BODY ~f:__let_syntax_loop__001_
            | false -> Let_syntax.return ())[@@ppxlib.do_not_enter_value ] in
    __let_syntax_loop__001_ ()
    ----
    locality = local:
    let rec __let_syntax_loop__002_ () =
      local_ let __nontail__005_ =
               Let_syntax.bind MY_CONDITION
                 ~f:(fun __let_syntax__003_ ->
                       local_ let __nontail__004_ =
                                match __let_syntax__003_ with
                                | true ->
                                    Let_syntax.bind MY_BODY
                                      ~f:__let_syntax_loop__002_
                                | false -> Let_syntax.return () in
                              __nontail__004_) in
             __nontail__005_[@@ppxlib.do_not_enter_value ] in
    __let_syntax_loop__002_ ()
    |}]
;;

let%expect_test "while%bind trivial test" =
  let i = ref 0 in
  while%bind.Monad.Ident
    incr i;
    !i <= 5
  do
    printf "%d\n" !i
  done;
  [%expect {|
    1
    2
    3
    4
    5
    |}]
;;

let%expect_test "monadic use" =
  let open Or_error.Let_syntax in
  let next i = if i < 5 then Ok (i + 1) else error_s [%message "too big"] in
  let t n =
    let i = ref 0 in
    let result =
      while%bind
        let%map i' = next !i in
        i := i';
        !i <= n
      do
        printf "%d\n" !i;
        Ok ()
      done
    in
    print_s [%sexp (result : unit Or_error.t)]
  in
  t 3;
  [%expect {|
    1
    2
    3
    (Ok ())
    |}];
  t 10;
  [%expect {|
    1
    2
    3
    4
    5
    (Error "too big")
    |}]
;;
