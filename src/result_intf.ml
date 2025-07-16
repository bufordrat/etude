module type AUGMENTED = sig
  open Endofunctors_intf

  type 'a t

  include Monad.AUGMENTED with type 'a t := 'a t

  include
    Traverse_intf.Traversable.List.AUGMENTED
      with type 'a t := 'a t
end

module type ERROR = sig
  type t
end

module type MAKE = functor (E : ERROR) -> sig
  type 'a t = ('a, E.t) result

  include AUGMENTED with type 'a t := ('a, E.t) result

  (* Etude values that are polymorphic go here *)

  val bind_error :
    ('a, 'e) result ->
    ('e -> ('a, 'd) result) ->
    ('a, 'd) result

  val oks : ('a, 'b) result list -> 'a list
  val errors : ('a, 'b) result list -> 'b list

  (* Prelude values that are polymorphic go here. *)

  val ok : 'a -> ('a, 'd) result
  val good : ('a, 'd) result -> bool
  val bad : ('a, 'd) result -> bool
  val get_ok : ('a, 'd) result -> 'a
  val default : 'a -> ('a, 'd) result -> 'a
  val reduce : ('a, 'd) result list -> 'a list
  val always : 'a -> (unit -> 'b) -> 'b
  val to_bool : ('a, 'd) result -> bool
  val error : 'd -> ('a, 'd) result
  val get_error : ('a, 'd) result -> 'd

  val on_error :
    ('a, 'd) result ->
    ('d -> ('a, 'c) result) ->
    ('a, 'c) result

  val ( >>/ ) :
    ('a, 'd) result ->
    ('d -> ('a, 'c) result) ->
    ('a, 'c) result

  val ( or ) :
    ('a, 'd) result -> ('a, 'c) result -> ('a, 'c) result

  val trap :
    (exn -> 'd) -> ('b -> 'c) -> 'b -> ('c, 'd) result

  val trapc : 'd -> ('b -> 'c) -> 'b -> ('c, 'd) result

  val witherr :
    ('a -> 'd) -> ('c, 'a) result -> ('c, 'd) result

  val witherrc : 'd -> ('b, 'c) result -> ('b, 'd) result
  val of_bool : ?err:'d -> 'd -> bool -> ('d, 'd) result
  val of_option : 'd -> 'b option -> ('b, 'd) result
  val some_error : ('a, 'd) result -> 'd option

  (* Stdlib values that are polymorphic go here *)

  val value : ('a, 'd) result -> default:'a -> 'a
  val to_option : ('a, 'd) result -> 'a option
  val to_list : ('a, 'd) result -> 'a list
  val to_seq : ('a, 'd) result -> 'a Seq.t

  val map_error :
    ('e -> 'd) -> ('a, 'e) result -> ('a, 'd) result

  val fold :
    ok:('a -> 'c) ->
    error:('d -> 'c) ->
    ('a, 'd) result ->
    'c

  val equal :
    ok:('a -> 'a -> bool) ->
    error:('d -> 'd -> bool) ->
    ('a, 'd) result ->
    ('a, 'd) result ->
    bool

  val compare :
    ok:('a -> 'a -> int) ->
    error:('d -> 'd -> int) ->
    ('a, 'd) result ->
    ('a, 'd) result ->
    int
end
