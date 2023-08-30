(** A local option monad, to demonstrate [let%mapl] and [let%bindl]. *)

type 'a t = 'a option

module Let_syntax : sig
  module Let_syntax : sig
    val return : ('a[@local]) -> ('a t[@local])

    val map
      :  ('a t[@local])
      -> f:((('a[@local]) -> ('b[@local]))[@local])
      -> ('b t[@local])

    val bind
      :  ('a t[@local])
      -> f:((('a[@local]) -> ('b t[@local]))[@local])
      -> ('b t[@local])

    val both : ('a t[@local]) -> ('b t[@local]) -> (('a * 'b) t[@local])

    val map4
      :  ('a t[@local])
      -> ('b t[@local])
      -> ('c t[@local])
      -> ('d t[@local])
      -> f:
           ((('a[@local]) -> ('b[@local]) -> ('c[@local]) -> ('d[@local]) -> ('e[@local]))
           [@local])
      -> ('e t[@local])

    val bind4
      :  ('a t[@local])
      -> ('b t[@local])
      -> ('c t[@local])
      -> ('d t[@local])
      -> f:
           ((('a[@local])
             -> ('b[@local])
             -> ('c[@local])
             -> ('d[@local])
             -> ('e t[@local]))
           [@local])
      -> ('e t[@local])

    module Open_on_rhs : sig end
  end
end
