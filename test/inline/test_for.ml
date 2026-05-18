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

let%expect_test "for%bind (upto) expansion" =
  expand
    Ppx_let_expander.bind
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    [%expr
      for i = MY_START to MY_STOP do
        MY_BODY
      done];
  [%expect
    {|
    locality = ((allocate_function_on_stack false) (return_value_in_exclave false)):
    let i = MY_START[@@ppxlib.do_not_enter_value ]
    and __let_syntax_for_loop_bound__001_ = MY_STOP[@@ppxlib.do_not_enter_value ] in
    if i <= __let_syntax_for_loop_bound__001_
    then
      let rec __let_syntax_loop__002_ i =
        Let_syntax.bind MY_BODY
          ~f:(fun () ->
                if i < __let_syntax_for_loop_bound__001_
                then
                  let i = i + 1[@@ppxlib.do_not_enter_value ] in
                  __let_syntax_loop__002_ i
                else Let_syntax.return ())[@@ppxlib.do_not_enter_value ] in
      __let_syntax_loop__002_ i
    else Let_syntax.return ()
    ----
    locality = ((allocate_function_on_stack true) (return_value_in_exclave false)):
    let i = MY_START[@@ppxlib.do_not_enter_value ]
    and __let_syntax_for_loop_bound__003_ = MY_STOP[@@ppxlib.do_not_enter_value ] in
    if i <= __let_syntax_for_loop_bound__003_
    then
      let rec __let_syntax_loop__004_ =
        (fun i ->
           ((Let_syntax.bind MY_BODY
               ~f:(fun () ->
                     if i < __let_syntax_for_loop_bound__003_
                     then
                       let i = i + 1[@@ppxlib.do_not_enter_value ] in
                       __let_syntax_loop__004_ i
                     else Let_syntax.return () : @ local))
           [@nontail ]) : @ local)[@@ppxlib.do_not_enter_value ] in
      ((__let_syntax_loop__004_ i)[@nontail ])
    else Let_syntax.return ()
    ----
    locality = ((allocate_function_on_stack false) (return_value_in_exclave true)):
    let i = MY_START[@@ppxlib.do_not_enter_value ]
    and __let_syntax_for_loop_bound__005_ = MY_STOP[@@ppxlib.do_not_enter_value ] in
    if i <= __let_syntax_for_loop_bound__005_
    then
      let rec __let_syntax_loop__006_ i =
        exclave_ Let_syntax.bind MY_BODY
                   ~f:(fun () ->
                         exclave_ if i < __let_syntax_for_loop_bound__005_
                                  then
                                    let i = i + 1[@@ppxlib.do_not_enter_value ] in
                                    __let_syntax_loop__006_ i
                                  else Let_syntax.return ())[@@ppxlib.do_not_enter_value
                                                              ] in
      __let_syntax_loop__006_ i
    else Let_syntax.return ()
    ----
    locality = ((allocate_function_on_stack true) (return_value_in_exclave true)):
    let i = MY_START[@@ppxlib.do_not_enter_value ]
    and __let_syntax_for_loop_bound__007_ = MY_STOP[@@ppxlib.do_not_enter_value ] in
    if i <= __let_syntax_for_loop_bound__007_
    then
      let rec __let_syntax_loop__008_ =
        (fun i ->
           exclave_ ((Let_syntax.bind MY_BODY
                        ~f:(fun () ->
                              exclave_ if i < __let_syntax_for_loop_bound__007_
                                       then
                                         let i = i + 1[@@ppxlib.do_not_enter_value
                                                        ] in
                                         __let_syntax_loop__008_ i
                                       else Let_syntax.return () : @ local))
             [@nontail ]) : @ local)[@@ppxlib.do_not_enter_value ] in
      ((__let_syntax_loop__008_ i)[@nontail ])
    else Let_syntax.return ()
    |}]
;;

let%expect_test "for%bind (downto) expansion" =
  expand
    Ppx_let_expander.bind
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    [%expr
      for i = MY_START downto MY_STOP do
        MY_BODY
      done];
  [%expect
    {|
    locality = ((allocate_function_on_stack false) (return_value_in_exclave false)):
    let i = MY_START[@@ppxlib.do_not_enter_value ]
    and __let_syntax_for_loop_bound__009_ = MY_STOP[@@ppxlib.do_not_enter_value ] in
    if i >= __let_syntax_for_loop_bound__009_
    then
      let rec __let_syntax_loop__010_ i =
        Let_syntax.bind MY_BODY
          ~f:(fun () ->
                if i > __let_syntax_for_loop_bound__009_
                then
                  let i = i - 1[@@ppxlib.do_not_enter_value ] in
                  __let_syntax_loop__010_ i
                else Let_syntax.return ())[@@ppxlib.do_not_enter_value ] in
      __let_syntax_loop__010_ i
    else Let_syntax.return ()
    ----
    locality = ((allocate_function_on_stack true) (return_value_in_exclave false)):
    let i = MY_START[@@ppxlib.do_not_enter_value ]
    and __let_syntax_for_loop_bound__011_ = MY_STOP[@@ppxlib.do_not_enter_value ] in
    if i >= __let_syntax_for_loop_bound__011_
    then
      let rec __let_syntax_loop__012_ =
        (fun i ->
           ((Let_syntax.bind MY_BODY
               ~f:(fun () ->
                     if i > __let_syntax_for_loop_bound__011_
                     then
                       let i = i - 1[@@ppxlib.do_not_enter_value ] in
                       __let_syntax_loop__012_ i
                     else Let_syntax.return () : @ local))
           [@nontail ]) : @ local)[@@ppxlib.do_not_enter_value ] in
      ((__let_syntax_loop__012_ i)[@nontail ])
    else Let_syntax.return ()
    ----
    locality = ((allocate_function_on_stack false) (return_value_in_exclave true)):
    let i = MY_START[@@ppxlib.do_not_enter_value ]
    and __let_syntax_for_loop_bound__013_ = MY_STOP[@@ppxlib.do_not_enter_value ] in
    if i >= __let_syntax_for_loop_bound__013_
    then
      let rec __let_syntax_loop__014_ i =
        exclave_ Let_syntax.bind MY_BODY
                   ~f:(fun () ->
                         exclave_ if i > __let_syntax_for_loop_bound__013_
                                  then
                                    let i = i - 1[@@ppxlib.do_not_enter_value ] in
                                    __let_syntax_loop__014_ i
                                  else Let_syntax.return ())[@@ppxlib.do_not_enter_value
                                                              ] in
      __let_syntax_loop__014_ i
    else Let_syntax.return ()
    ----
    locality = ((allocate_function_on_stack true) (return_value_in_exclave true)):
    let i = MY_START[@@ppxlib.do_not_enter_value ]
    and __let_syntax_for_loop_bound__015_ = MY_STOP[@@ppxlib.do_not_enter_value ] in
    if i >= __let_syntax_for_loop_bound__015_
    then
      let rec __let_syntax_loop__016_ =
        (fun i ->
           exclave_ ((Let_syntax.bind MY_BODY
                        ~f:(fun () ->
                              exclave_ if i > __let_syntax_for_loop_bound__015_
                                       then
                                         let i = i - 1[@@ppxlib.do_not_enter_value
                                                        ] in
                                         __let_syntax_loop__016_ i
                                       else Let_syntax.return () : @ local))
             [@nontail ]) : @ local)[@@ppxlib.do_not_enter_value ] in
      ((__let_syntax_loop__016_ i)[@nontail ])
    else Let_syntax.return ()
    |}]
;;

let%expect_test "for%bind wildcard pattern expansion" =
  expand
    Ppx_let_expander.bind
    Ppx_let_expander.Extension_kind.default
    ~modul:None
    [%expr
      for _ = MY_START to MY_STOP do
        MY_BODY
      done];
  [%expect
    {|
    locality = ((allocate_function_on_stack false) (return_value_in_exclave false)):
    let __let_syntax_idx__018_ = MY_START[@@ppxlib.do_not_enter_value ]
    and __let_syntax_for_loop_bound__017_ = MY_STOP[@@ppxlib.do_not_enter_value ] in
    if __let_syntax_idx__018_ <= __let_syntax_for_loop_bound__017_
    then
      let rec __let_syntax_loop__019_ __let_syntax_idx__018_ =
        Let_syntax.bind MY_BODY
          ~f:(fun () ->
                if __let_syntax_idx__018_ < __let_syntax_for_loop_bound__017_
                then
                  let __let_syntax_idx__018_ = __let_syntax_idx__018_ + 1
                    [@@ppxlib.do_not_enter_value ] in
                  __let_syntax_loop__019_ __let_syntax_idx__018_
                else Let_syntax.return ())[@@ppxlib.do_not_enter_value ] in
      __let_syntax_loop__019_ __let_syntax_idx__018_
    else Let_syntax.return ()
    ----
    locality = ((allocate_function_on_stack true) (return_value_in_exclave false)):
    let __let_syntax_idx__021_ = MY_START[@@ppxlib.do_not_enter_value ]
    and __let_syntax_for_loop_bound__020_ = MY_STOP[@@ppxlib.do_not_enter_value ] in
    if __let_syntax_idx__021_ <= __let_syntax_for_loop_bound__020_
    then
      let rec __let_syntax_loop__022_ =
        (fun __let_syntax_idx__021_ ->
           ((Let_syntax.bind MY_BODY
               ~f:(fun () ->
                     if
                       __let_syntax_idx__021_ < __let_syntax_for_loop_bound__020_
                     then
                       let __let_syntax_idx__021_ = __let_syntax_idx__021_ + 1
                         [@@ppxlib.do_not_enter_value ] in
                       __let_syntax_loop__022_ __let_syntax_idx__021_
                     else Let_syntax.return () : @ local))
           [@nontail ]) : @ local)[@@ppxlib.do_not_enter_value ] in
      ((__let_syntax_loop__022_ __let_syntax_idx__021_)[@nontail ])
    else Let_syntax.return ()
    ----
    locality = ((allocate_function_on_stack false) (return_value_in_exclave true)):
    let __let_syntax_idx__024_ = MY_START[@@ppxlib.do_not_enter_value ]
    and __let_syntax_for_loop_bound__023_ = MY_STOP[@@ppxlib.do_not_enter_value ] in
    if __let_syntax_idx__024_ <= __let_syntax_for_loop_bound__023_
    then
      let rec __let_syntax_loop__025_ __let_syntax_idx__024_ =
        exclave_ Let_syntax.bind MY_BODY
                   ~f:(fun () ->
                         exclave_ if
                                    __let_syntax_idx__024_ <
                                      __let_syntax_for_loop_bound__023_
                                  then
                                    let __let_syntax_idx__024_ =
                                      __let_syntax_idx__024_ + 1[@@ppxlib.do_not_enter_value
                                                                  ] in
                                    __let_syntax_loop__025_
                                      __let_syntax_idx__024_
                                  else Let_syntax.return ())[@@ppxlib.do_not_enter_value
                                                              ] in
      __let_syntax_loop__025_ __let_syntax_idx__024_
    else Let_syntax.return ()
    ----
    locality = ((allocate_function_on_stack true) (return_value_in_exclave true)):
    let __let_syntax_idx__027_ = MY_START[@@ppxlib.do_not_enter_value ]
    and __let_syntax_for_loop_bound__026_ = MY_STOP[@@ppxlib.do_not_enter_value ] in
    if __let_syntax_idx__027_ <= __let_syntax_for_loop_bound__026_
    then
      let rec __let_syntax_loop__028_ =
        (fun __let_syntax_idx__027_ ->
           exclave_ ((Let_syntax.bind MY_BODY
                        ~f:(fun () ->
                              exclave_ if
                                         __let_syntax_idx__027_ <
                                           __let_syntax_for_loop_bound__026_
                                       then
                                         let __let_syntax_idx__027_ =
                                           __let_syntax_idx__027_ + 1[@@ppxlib.do_not_enter_value
                                                                       ] in
                                         __let_syntax_loop__028_
                                           __let_syntax_idx__027_
                                       else Let_syntax.return () : @ local))
             [@nontail ]) : @ local)[@@ppxlib.do_not_enter_value ] in
      ((__let_syntax_loop__028_ __let_syntax_idx__027_)[@nontail ])
    else Let_syntax.return ()
    |}]
;;

let%expect_test "for%bind trivial test" =
  for%bind.Monad.Ident i = 1 to 5 do
    printf "%d\n" i
  done;
  [%expect
    {|
    1
    2
    3
    4
    5
    |}]
;;

let%expect_test "for%bind downto trivial test" =
  for%bind.Monad.Ident i = 5 downto 1 do
    printf "%d\n" i
  done;
  [%expect
    {|
    5
    4
    3
    2
    1
    |}]
;;

let%expect_test "for%bind empty range" =
  for%bind.Monad.Ident i = 3 to 1 do
    printf "should not print: %d\n" i
  done;
  [%expect {| |}];
  for%bind.Monad.Ident i = 1 downto 3 do
    printf "should not print: %d\n" i
  done;
  [%expect {| |}]
;;

let%expect_test "for%bind wildcard pattern" =
  let count = ref 0 in
  for%bind.Monad.Ident _ = 1 to 4 do
    incr count;
    printf "tick %d\n" !count
  done;
  [%expect
    {|
    tick 1
    tick 2
    tick 3
    tick 4
    |}];
  for%bind.Monad.Ident _ = 3 downto 1 do
    printf "tock\n"
  done;
  [%expect
    {|
    tock
    tock
    tock
    |}]
;;

let%expect_test "monadic use" =
  let open Or_error.Let_syntax in
  let t n =
    let result =
      for%bind i = 1 to n do
        if i > 3
        then error_s [%message "too big" (i : int)]
        else (
          printf "%d\n" i;
          Ok ())
      done
    in
    print_s [%sexp (result : unit Or_error.t)]
  in
  t 3;
  [%expect
    {|
    1
    2
    3
    (Ok ())
    |}];
  t 10;
  [%expect
    {|
    1
    2
    3
    (Error ("too big" (i 4)))
    |}]
;;

let%expect_test "for%bind short-circuits on [None]" =
  let open Option.Let_syntax in
  let t n =
    let result =
      for%bind i = 1 to n do
        if i > 3
        then None
        else (
          printf "%d\n" i;
          Some ())
      done
    in
    print_s [%sexp (result : unit option)]
  in
  t 3;
  [%expect
    {|
    1
    2
    3
    (())
    |}];
  t 10;
  [%expect
    {|
    1
    2
    3
    ()
    |}]
;;

let%expect_test "for%bind downto short-circuits on [None]" =
  let open Option.Let_syntax in
  let result =
    for%bind i = 5 downto 1 do
      if i < 3
      then None
      else (
        printf "%d\n" i;
        Some ())
    done
  in
  print_s [%sexp (result : unit option)];
  [%expect
    {|
    5
    4
    3
    ()
    |}]
;;

let%expect_test "for%bind matches native [for] at the integer boundaries" =
  (* Regression test for the loop shape at [max_int] / [min_int]: the expansion uses a
     strict comparison against the bound before taking the recursive step so that
     [for%bind i = ... to max_int] terminates on the same iteration as native [for],
     without computing [i + 1] and overflowing to [min_int].

     As a safety net in case this ever regresses, we bound the monadic loop by
     short-circuiting via [Option] after more iterations than the native loop produced. *)
  let observe_native ~start ~stop =
    let seen = ref [] in
    for i = start to stop do
      seen := i :: !seen
    done;
    List.rev !seen
  in
  let observe_monadic ~start ~stop ~iteration_cap =
    let open Option.Let_syntax in
    let seen = ref [] in
    let count = ref 0 in
    let (_ : unit option) =
      for%bind i = start to stop do
        incr count;
        seen := i :: !seen;
        if !count > iteration_cap then None else Some ()
      done
    in
    List.rev !seen
  in
  let compare ~start ~stop =
    let native = observe_native ~start ~stop in
    (* Give the monadic loop room to go past the native bound before we force it to
       short-circuit. *)
    let monadic = observe_monadic ~start ~stop ~iteration_cap:(List.length native + 5) in
    print_s [%message (native : int list) (monadic : int list)]
  in
  compare ~start:(Int.max_value - 3) ~stop:Int.max_value;
  [%expect
    {|
    ((native
      (4611686018427387900 4611686018427387901 4611686018427387902
       4611686018427387903))
     (monadic
      (4611686018427387900 4611686018427387901 4611686018427387902
       4611686018427387903)))
    |}];
  compare ~start:Int.min_value ~stop:(Int.min_value + 3);
  [%expect
    {|
    ((native
      (-4611686018427387904 -4611686018427387903 -4611686018427387902
       -4611686018427387901))
     (monadic
      (-4611686018427387904 -4611686018427387903 -4611686018427387902
       -4611686018427387901)))
    |}];
  (* [downto] has the symmetric issue at [min_int]. *)
  let observe_native_downto ~start ~stop =
    let seen = ref [] in
    for i = start downto stop do
      seen := i :: !seen
    done;
    List.rev !seen
  in
  let observe_monadic_downto ~start ~stop ~iteration_cap =
    let open Option.Let_syntax in
    let seen = ref [] in
    let count = ref 0 in
    let (_ : unit option) =
      for%bind i = start downto stop do
        incr count;
        seen := i :: !seen;
        if !count > iteration_cap then None else Some ()
      done
    in
    List.rev !seen
  in
  let native_down =
    observe_native_downto ~start:(Int.min_value + 3) ~stop:Int.min_value
  in
  let monadic_down =
    observe_monadic_downto
      ~start:(Int.min_value + 3)
      ~stop:Int.min_value
      ~iteration_cap:(List.length native_down + 5)
  in
  print_s [%message (native_down : int list) (monadic_down : int list)];
  [%expect
    {|
    ((native_down
      (-4611686018427387901 -4611686018427387902 -4611686018427387903
       -4611686018427387904))
     (monadic_down
      (-4611686018427387901 -4611686018427387902 -4611686018427387903
       -4611686018427387904)))
    |}]
;;

let%expect_test "bounds are evaluated once" =
  (* Both the starting value and the upper/lower bound should be evaluated exactly once,
     regardless of how many iterations the loop runs. *)
  let counted name n =
    let count = ref 0 in
    let compute () =
      incr count;
      n
    in
    compute, fun () -> printf "%s evaluated %d time(s)\n" name !count
  in
  let start, print_start = counted "start" 1 in
  let stop, print_stop = counted "stop" 3 in
  for%bind.Monad.Ident i = start () to stop () do
    printf "%d\n" i
  done;
  print_start ();
  print_stop ();
  [%expect
    {|
    1
    2
    3
    start evaluated 1 time(s)
    stop evaluated 1 time(s)
    |}];
  let start, print_start = counted "start" 3 in
  let stop, print_stop = counted "stop" 1 in
  for%bind.Monad.Ident i = start () downto stop () do
    printf "%d\n" i
  done;
  print_start ();
  print_stop ();
  [%expect
    {|
    3
    2
    1
    start evaluated 1 time(s)
    stop evaluated 1 time(s)
    |}];
  (* Even if the loop runs zero times, both bounds must still be evaluated exactly once
     (matching the semantics of a plain [for] loop). *)
  let start, print_start = counted "start" 5 in
  let stop, print_stop = counted "stop" 1 in
  for%bind.Monad.Ident _ = start () to stop () do
    printf "should not print\n"
  done;
  print_start ();
  print_stop ();
  [%expect
    {|
    start evaluated 1 time(s)
    stop evaluated 1 time(s)
    |}]
;;
