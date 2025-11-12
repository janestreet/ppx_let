open! Base

[@@@warning "-32-34-60"]

module Identity_let_syntax = struct
  type 'a t = 'a

  let[@zero_alloc] return x = x

  module Let_syntax = struct
    let return = return

    let[@zero_alloc] bind x ~(f_zero_alloc @ local) = exclave_
      (f_zero_alloc [@zero_alloc assume]) x [@nontail]
    ;;

    let[@zero_alloc] map x ~(f_zero_alloc @ local) = exclave_
      (f_zero_alloc [@zero_alloc assume]) x [@nontail]
    ;;

    let both x y = x, y

    let[@zero_alloc] map2 x y ~(f_zero_alloc @ local) = exclave_
      (f_zero_alloc [@zero_alloc assume]) x y [@nontail]
    ;;

    let[@zero_alloc] bind2 x y ~(f_zero_alloc @ local) = exclave_
      (f_zero_alloc [@zero_alloc assume]) x y [@nontail]
    ;;

    module Open_on_rhs = struct
      let[@zero_alloc] return x = return x
    end
  end
end

open Identity_let_syntax

[@@@expand_inline
  (* Test basic let%bindz - this should not allocate *)
  let[@zero_alloc] test_bindz x =
    let%bindz y = x in
    return (y + 1)
  ;;

  (* Test basic let%mapz - this should not allocate *)
  let[@zero_alloc] test_mapz x =
    let%mapz y = x in
    y + 1
  ;;

  (* Test let%bindnz with multiple bindings - this should not allocate *)
  let[@zero_alloc] test_bindnz x y =
    let%bindnz a = x
    and b = y in
    return (a + b)
  ;;

  (* Test let%mapnz with multiple bindings - this should not allocate *)
  let[@zero_alloc] test_mapnz x y =
    let%mapnz a = x
    and b = y in
    a + b
  ;;

  (* Test let%bindzl with multiple bindings - this should not allocate *)
  let test_bindzl x y =
    let%bindzl a = x
    and b = y in
    a + b
  ;;

  (* Test let%mapzl with multiple bindings - this should not allocate *)
  let test_mapzl x y =
    let%mapzl a = x
    and b = y in
    a + b
  ;;

  (* Test let%bindnzl with multiple bindings - this should not allocate *)
  let[@zero_alloc] test_bindnzl x y =
    let%bindnzl a = x
    and b = y in
    a + b
  ;;

  (* Test let%mapzl with multiple bindings - this should not allocate *)
  let[@zero_alloc] test_mapnzl x y =
    let%mapnzl a = x
    and b = y in
    a + b
  ;;

  (* Test match%bindz - this should not allocate *)
  let[@zero_alloc] test_match_bindz x =
    match%bindz x with
    | true -> return 1
    | false -> return 0
  ;;

  (* Test match%mapz - this should not allocate *)
  let[@zero_alloc] test_match_mapz x =
    match%mapz x with
    | true -> 1
    | false -> 0
  ;;

  (* Test if%bindz - this should not allocate *)
  let[@zero_alloc] test_if_bindz cond = if%bindz cond then return 1 else return 0

  (* Test if%mapz - this should not allocate *)
  let[@zero_alloc] test_if_mapz cond = if%mapz cond then 1 else 0

  (* Test function%bindz - this should not allocate *)
  let[@zero_alloc] test_function_bindz =
    function%bindz
    | true -> return 1
    | false -> return 0
  ;;

  (* Test function%mapz - this should not allocate *)
  let[@zero_alloc] test_function_mapz =
    function%mapz
    | true -> 1
    | false -> 0
  ;;]

let test_bindz x = Let_syntax.bind x ~f_zero_alloc:(fun [@zero_alloc] y -> return (y + 1))
[@@zero_alloc]
;;

let test_mapz x = Let_syntax.map x ~f_zero_alloc:(fun [@zero_alloc] y -> y + 1)
[@@zero_alloc]
;;

let test_bindnz x y =
  let __let_syntax__003_ = x [@@ppxlib.do_not_enter_value]
  and __let_syntax__004_ = y [@@ppxlib.do_not_enter_value] in
  Let_syntax.bind2
    __let_syntax__003_
    __let_syntax__004_
    ~f_zero_alloc:(fun [@zero_alloc] a b -> return (a + b))
[@@zero_alloc]
;;

let test_mapnz x y =
  let __let_syntax__007_ = x [@@ppxlib.do_not_enter_value]
  and __let_syntax__008_ = y [@@ppxlib.do_not_enter_value] in
  Let_syntax.map2
    __let_syntax__007_
    __let_syntax__008_
    ~f_zero_alloc:(fun [@zero_alloc] a b -> a + b)
[@@zero_alloc]
;;

let test_bindzl x y =
  let __let_syntax__011_ = x [@@ppxlib.do_not_enter_value]
  and __let_syntax__012_ = y [@@ppxlib.do_not_enter_value] in
  Let_syntax.bind
    (Let_syntax.both __let_syntax__011_ __let_syntax__012_)
    ~f_zero_alloc:(local_ fun [@zero_alloc] (a, b) -> exclave_ a + b)
  [@nontail]
;;

let test_mapzl x y =
  let __let_syntax__014_ = x [@@ppxlib.do_not_enter_value]
  and __let_syntax__015_ = y [@@ppxlib.do_not_enter_value] in
  Let_syntax.map
    (Let_syntax.both __let_syntax__014_ __let_syntax__015_)
    ~f_zero_alloc:(local_ fun [@zero_alloc] (a, b) -> exclave_ a + b)
  [@nontail]
;;

let test_bindnzl x y =
  let __let_syntax__017_ = x [@@ppxlib.do_not_enter_value]
  and __let_syntax__018_ = y [@@ppxlib.do_not_enter_value] in
  Let_syntax.bind2
    __let_syntax__017_
    __let_syntax__018_
    ~f_zero_alloc:(local_ fun [@zero_alloc] a b -> exclave_ a + b)
  [@nontail]
[@@zero_alloc]
;;

let test_mapnzl x y =
  let __let_syntax__021_ = x [@@ppxlib.do_not_enter_value]
  and __let_syntax__022_ = y [@@ppxlib.do_not_enter_value] in
  Let_syntax.map2
    __let_syntax__021_
    __let_syntax__022_
    ~f_zero_alloc:(local_ fun [@zero_alloc] a b -> exclave_ a + b)
  [@nontail]
[@@zero_alloc]
;;

let test_match_bindz x =
  Let_syntax.bind x ~f_zero_alloc:(function [@zero_alloc]
    | true -> return 1
    | false -> return 0)
[@@zero_alloc]
;;

let test_match_mapz x =
  Let_syntax.map x ~f_zero_alloc:(function [@zero_alloc]
    | true -> 1
    | false -> 0)
[@@zero_alloc]
;;

let test_if_bindz cond =
  Let_syntax.bind cond ~f_zero_alloc:(function [@zero_alloc]
    | true -> return 1
    | false -> return 0)
[@@zero_alloc]
;;

let test_if_mapz cond =
  Let_syntax.map cond ~f_zero_alloc:(function [@zero_alloc]
    | true -> 1
    | false -> 0)
[@@zero_alloc]
;;

let test_function_bindz __let_syntax__025_ =
  Let_syntax.bind __let_syntax__025_ ~f_zero_alloc:(function [@zero_alloc]
    | true -> return 1
    | false -> return 0)
[@@zero_alloc]
;;

let test_function_mapz __let_syntax__026_ =
  Let_syntax.map __let_syntax__026_ ~f_zero_alloc:(function [@zero_alloc]
    | true -> 1
    | false -> 0)
[@@zero_alloc]
;;

[@@@end]
