module Monad_example = struct

  module X : sig
    type 'a t
    module Let_syntax : sig
      val return : 'a -> 'a t
      val bind   : 'a t -> ('a -> 'b t) -> 'b t
      val map    : 'a t -> f:('a -> 'b) -> 'b t
      val both   : 'a t -> 'b t -> ('a * 'b) t
      module Open_on_rhs  : sig val return : 'a -> 'a t end
      module Open_in_body : sig val return : 'a -> 'a t end
    end
  end = struct
    type 'a t = 'a
    let return x = x
    let bind x f = f x
    let map x ~f = f x
    let both x y = (x, y)
    module Let_syntax = struct
      let return = return
      let bind   = bind
      let map    = map
      let both   = both
      module Open_on_rhs  = struct let return = return end
      module Open_in_body = struct let return = return end
    end
  end

  module Let_syntax = X.Let_syntax

  let _mf a : _ X.t =
    let%bind_open x = a in
    return (x + 1)

  let _mf' a b c : _ X.t =
    let%bind_open x = a and y = b and (u, v) = c in
    return (x + y + (u * v))

  let _mg a : _ X.t =
    let%map x = a in
    x + 1

  let _mg' a b c : _ X.t =
    let%map x = a and y = b and (u, v) = c in
    x + y + (u * v)

  let _mh a : _ X.t =
    match%bind_open a with
    | 0 -> return true
    | _ -> return false

  let _mi a : _ X.t =
    match%map a with
    | 0 -> true
    | _ -> false
end

module Applicative_example = struct

  module X : sig
    type 'a t
    module Let_syntax : sig
      val return : 'a -> 'a t
      val map    : 'a t -> f:('a -> 'b) -> 'b t
      val both   : 'a t -> 'b t -> ('a * 'b) t
      module Open_on_rhs : sig
        val flag : int t
        val anon : int t
      end
      module Open_in_body : sig end
    end
  end = struct
    type 'a t = 'a
    let return x = x
    let map x ~f = f x
    let both x y = (x, y)
    module Let_syntax = struct
      let return = return
      let map    = map
      let both   = both
      module Open_on_rhs = struct
        let flag = 66
        let anon = 77
      end
      module Open_in_body = struct end
    end
  end

  module Let_syntax = X.Let_syntax

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

  let _ag' a b c : _ X.t =
    let%map x = a and y = b and (u, v) = c in
    x + y + (u * v)

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
end

module Async_command_override_example = struct

  module Deferred : sig
    type 'a t
    val return : 'a -> 'a t
    module Let_syntax : sig
      type 'a t
      val return : 'a -> 'a t
      val map    : 'a t -> f:('a -> 'b) -> 'b t
      val both   : 'a t -> 'b t -> ('a * 'b) t
      module Open_on_rhs  : sig val return : 'a -> 'a t end
      module Open_in_body : sig val return : 'a -> 'a t end
    end with type 'a t := 'a t
  end = struct
    type 'a t = 'a
    let return x = x
    module Let_syntax = struct
      let return = return
      let map x ~f = f x
      let both x y = (x, y)
      module Open_on_rhs  = struct let return = return end
      module Open_in_body = struct let return = return end
    end
  end

  module Command : sig
    module Param : sig
      type 'a t
      val return : 'a -> 'a t
      val flag : 'a -> int t
      val anon : 'a -> int t
    end
    module Let_syntax : sig
      type 'a t
      val return : 'a -> 'a t
      val map    : 'a t -> f:('a -> 'b) -> 'b t
      val both   : 'a t -> 'b t -> ('a * 'b) t
      module Open_on_rhs = Param
      module Open_in_body : sig end
    end with type 'a t := 'a Param.t
  end = struct
    module Param = struct
      type 'a t = 'a
      let return x = x
      let map x ~f = f x
      let both x y = (x, y)
      let flag _ = 66
      let anon _ = 77
    end
    module Let_syntax = struct
      include Param
      module Open_on_rhs = Param
      module Open_in_body = struct end
    end
  end

  module Command_override = struct
    module Param = struct
      include Command.Param
      let special_flag = flag 88
    end
    module Let_syntax = struct
      include (Command.Let_syntax : module type of Command.Let_syntax
               with module Open_on_rhs  := Command.Let_syntax.Open_on_rhs
                and module Open_in_body := Command.Let_syntax.Open_in_body)

      module Open_on_rhs = Param
      module Open_in_body = struct
        module Let_syntax = Deferred.Let_syntax
      end
    end
  end

  let _1 : int Command.Param.t =
    let module Let_syntax = Command.Let_syntax in
    [%map_open
      let x = flag "foo"
      and y = anon "bar"
      in
      x + y
    ]
  ;;

  let _1 : (unit -> int Deferred.t) Command_override.Param.t =
    let module Let_syntax = Command_override.Let_syntax in
    [%map_open
      let x = flag "foo"
      and y = anon "bar"
      and z = special_flag
      in
      (fun () ->
         let%map () = Deferred.return () in
         x + y + z)
    ]
  ;;

  let _1 : (unit -> unit Deferred.t) Command.Param.t =
    let module Let_syntax = Command_override.Let_syntax in
    [%map_open
      let () = return () in
      fun () ->
        let%map () = Deferred.return () in
        ()
    ]
  ;;
end
