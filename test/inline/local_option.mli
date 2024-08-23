(** A local option monad, to demonstrate [let%mapl] and [let%bindl]. *)

type 'a t = 'a option

module Let_syntax : sig
  module Let_syntax : sig
    val return : local_ 'a -> local_ 'a t
    val map : local_ 'a t -> f:local_ (local_ 'a -> local_ 'b) -> local_ 'b t
    val bind : local_ 'a t -> f:local_ (local_ 'a -> local_ 'b t) -> local_ 'b t
    val both : local_ 'a t -> local_ 'b t -> local_ ('a * 'b) t

    val map4
      :  local_ 'a t
      -> local_ 'b t
      -> local_ 'c t
      -> local_ 'd t
      -> f:local_ (local_ 'a -> local_ 'b -> local_ 'c -> local_ 'd -> local_ 'e)
      -> local_ 'e t

    val bind4
      :  local_ 'a t
      -> local_ 'b t
      -> local_ 'c t
      -> local_ 'd t
      -> f:local_ (local_ 'a -> local_ 'b -> local_ 'c -> local_ 'd -> local_ 'e t)
      -> local_ 'e t

    module Open_on_rhs : sig end
  end
end
