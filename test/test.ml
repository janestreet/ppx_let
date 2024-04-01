module Monad_example = struct
  module X : sig
    type 'a t

    module Let_syntax : sig
      val return : 'a -> 'a t

      module Let_syntax : sig
        val return : 'a -> 'a t
        val bind : 'a t -> f:('a -> 'b t) -> 'b t
        val map : 'a t -> f:('a -> 'b) -> 'b t
        val both : 'a t -> 'b t -> ('a * 'b) t

        module Open_on_rhs : sig
          val return : 'a -> 'a t
        end
      end
    end
  end = struct
    type 'a t = 'a

    let return x = x
    let bind x ~f = f x
    let map x ~f = f x
    let both x y = x, y

    module Let_syntax = struct
      let return = return

      module Let_syntax = struct
        let return = return
        let bind = bind
        let map = map
        let both = both

        module Open_on_rhs = struct
          let return = return
        end
      end
    end
  end

  open X.Let_syntax

  let _mf a : _ X.t =
    let%bind_open x = a in
    return (x + 1)
  ;;

  let _mf' a b c : _ X.t =
    let%bind_open x = a
    and y = b
    and u, v = c in
    return (x + y + (u * v))
  ;;

  let _mg a : _ X.t =
    let%map x : int X.t = a in
    x + 1
  ;;

  let _mg' a b c : _ X.t =
    let%map x = a
    and y = b
    and u, v = c in
    x + y + (u * v)
  ;;

  let _mh a : _ X.t =
    match%bind_open a with
    | 0 -> return true
    | _ -> return false
  ;;

  let _mi a : _ X.t =
    match%map a with
    | 0 -> true
    | _ -> false
  ;;

  let _mif a : _ X.t = if%bind_open a then return true else return false
  let _mif' a : _ X.t = if%map a then true else false

  let _mj : int X.t -> bool X.t =
    function%bind
    | 0 -> return true
    | _ -> return false
  ;;

  let _mk : int X.t -> bool X.t =
    function%map
    | 0 -> true
    | _ -> false
  ;;
end

module Applicative_example = struct
  module X : sig
    type 'a t

    module Let_syntax : sig
      val return : 'a -> 'a t

      module Let_syntax : sig
        val return : 'a -> 'a t
        val map : 'a t -> f:('a -> 'b) -> 'b t
        val both : 'a t -> 'b t -> ('a * 'b) t

        module Open_on_rhs : sig
          val flag : int t
          val anon : int t
        end
      end
    end
  end = struct
    type 'a t = 'a

    let return x = x
    let map x ~f = f x
    let both x y = x, y

    module Let_syntax = struct
      let return = return

      module Let_syntax = struct
        let return = return
        let map = map
        let both = both

        module Open_on_rhs = struct
          let flag = 66
          let anon = 77
        end
      end
    end
  end

  open X.Let_syntax

  (* {[
       let _af a : _ X.t =
         let%bind x = a in (* "Error: Unbound value Let_syntax.bind" *)
         return (x + 1)
     ]} *)

  (* {[
       let _af' a b c : _ X.t =
         let%bind x = a and y = b and (u, v) = c in (* "Error: Unbound value Let_syntax.bind" *)
         return (x + y + (u * v))
     ]} *)

  let _ag a : _ X.t =
    let%map x = a in
    x + 1
  ;;

  let _ag' a b c : _ X.t =
    let%map x = a
    and y = b
    and u, v = c in
    x + y + (u * v)
  ;;

  (* {[
       let _ah a : _ X.t =
         match%bind a with (* "Error: Unbound value Let_syntax.bind" *)
         | 0 -> return true
         | _ -> return false
     ]} *)

  let _ai a : _ X.t =
    match%map a with
    | 0 -> true
    | _ -> false
  ;;
end

module Example_without_open = struct
  let _ag a : _ Applicative_example.X.t =
    let%map.Applicative_example.X x = a in
    x + 1
  ;;
end

module Example_with_mapn = struct
  module Let_syntax = struct
    let return = Monad_example.X.Let_syntax.return

    module Let_syntax = struct
      include Monad_example.X.Let_syntax.Let_syntax

      let map2 a b ~f = map (both a b) ~f:(fun (a, b) -> f a b)
      let map3 a b c ~f = map2 (both a b) c ~f:(fun (a, b) c -> f a b c)
      let map4 a b c d ~f = map2 (both a b) (both c d) ~f:(fun (a, b) (c, d) -> f a b c d)
    end
  end

  let _x =
    let open Let_syntax in
    let%mapn a = return 1
    and b = return "hi"
    and c = return 2.34
    and d = return true in
    Printf.sprintf "%d %s %f %b" a b c d
  ;;
end

let () =
  (* Use this code to test if :MerlinTypeOf behaves properly. In particular,
     [lhs_*] variables should have type 'a instead of 'a Monad_example.X.t. *)
  let open Monad_example.X.Let_syntax in
  let rhs_a = return 1 in
  let rhs_b = return 1. in
  let rhs_c = return 'c' in
  let (_ : _) =
    (* Non-parallel sequence of binds. *)
    let%bind lhs_a = rhs_a in
    let%bind lhs_b = rhs_b in
    let%bind lhs_c = rhs_c in
    return (lhs_a, lhs_b, lhs_c)
  in
  let tuple =
    (* Parallel bind. *)
    let%bind lhs_a = rhs_a
    and lhs_b = rhs_b
    and lhs_c = rhs_c in
    return (lhs_a, lhs_b, lhs_c)
  in
  let (_ : _) =
    (* Destructuring parallel bind. *)
    let%bind lhs_a, lhs_b, lhs_c = tuple
    and lhs_a', lhs_b', lhs_c' = tuple in
    return (lhs_a, lhs_b, lhs_c, lhs_a', lhs_b', lhs_c')
  in
  let (_ : _) =
    (* Body is a function *)
    let%map lhs_a, lhs_b, lhs_c = tuple in
    fun () -> lhs_c, lhs_b, lhs_a
  in
  ()
;;
