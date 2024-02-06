type 'a t = 'a option

let return x = Some x

let map t ~f =
  match t with
  | None -> None
  | Some x -> Some (f x)
;;

let bind t ~f =
  match t with
  | None -> None
  | Some x -> f x
;;

let both t1 t2 =
  match t1, t2 with
  | Some t1, Some t2 -> Some (t1, t2)
  | _ -> None
;;

let bind4 t1 t2 t3 t4 ~f =
  match t1, t2, t3, t4 with
  | Some t1, Some t2, Some t3, Some t4 -> f t1 t2 t3 t4
  | _ -> None
;;

let map4 t1 t2 t3 t4 ~f =
  match t1, t2, t3, t4 with
  | Some t1, Some t2, Some t3, Some t4 -> Some (f t1 t2 t3 t4)
  | _ -> None
;;

module Let_syntax = struct
  module Let_syntax = struct
    let return = return
    let map = map
    let bind = bind
    let both = both
    let map4 = map4
    let bind4 = bind4

    module Open_on_rhs = struct end
  end
end
