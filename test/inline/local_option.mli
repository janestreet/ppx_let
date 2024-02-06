(** A local option monad, to demonstrate [let%mapl] and [let%bindl]. *)

type 'a t = 'a option

module Let_syntax : sig
  module Let_syntax : sig
    val return : 'a -> 'a t
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val bind : 'a t -> f:('a -> 'b t) -> 'b t
    val both : 'a t -> 'b t -> ('a * 'b) t
    val map4 : 'a t -> 'b t -> 'c t -> 'd t -> f:('a -> 'b -> 'c -> 'd -> 'e) -> 'e t
    val bind4 : 'a t -> 'b t -> 'c t -> 'd t -> f:('a -> 'b -> 'c -> 'd -> 'e t) -> 'e t

    module Open_on_rhs : sig end
  end
end
