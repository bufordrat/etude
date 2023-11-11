module type STDLIB = sig
  include (module type of Stdlib.Option)
end

module type PRELUDE = sig
  type 'a t

    val return : 'a -> 'a option
    val default : 'a -> 'a option -> 'a
    val something : 'a option -> bool
    val nothing : 'a option -> bool
    val on_none : 'a option -> (unit -> 'a) -> 'a option
    val ( >>/ ) : 'a option -> (unit -> 'a) -> 'a option
    val reduce : 'a option list -> 'a list
    val foldl : ('a -> 'b -> 'a) -> 'a -> 'b option -> 'a
    val foldr : ('a -> 'b -> 'b) -> 'b -> 'a option -> 'b
    val either : ('a -> 'b) -> 'b -> 'a option -> 'b
    val maybe : ('a -> unit) -> 'a option -> unit
    val call : 'a -> ('b -> 'a) option -> 'b -> 'a
    val catch : ?this:exn -> ('a -> 'b) -> 'a -> 'b option
    val to_bool : 'a option -> bool
    val to_list : 'a option -> 'a list
    val to_exn : exn -> 'a option -> 'a
    val of_result : ('a, 'b) result -> 'a option
    val to_string : ('a -> string) -> 'a option -> string
    val print : ('a -> string) -> 'a option -> unit
    val random : (unit -> 'a) -> unit -> 'a option
end

module type ETUDE = sig
  open Endofunctors_intf

  type 'a t
  include Monad.AUGMENTED with type 'a t := 'a t
  include Monoid_intf.MONOID with type 'a t := 'a t
  val cat_options : 'a t list -> 'a list
end

module type AUGMENTED = sig
  include STDLIB
  include PRELUDE with type 'a t := 'a t
  include ETUDE with type 'a t := 'a t
end
