module type STDLIB = sig
  include (module type of Stdlib.List)
end

module type PRELUDE = sig
  type 'a t
  val len : 'a list -> int
  val comparison : ('a -> 'a -> int) -> 'a list -> 'a list -> int
  val to_string :
    ?left:string ->
    ?sep:string -> ?right:string -> ('a -> string) -> 'a list -> string
  val head : 'a list -> 'a option
  val tail : 'a list -> 'a list option
  val last : 'a list -> 'a
  val get : int -> 'a list -> 'a
  val snoc : 'a list -> 'a -> 'a list
  val consup : 'a -> 'a list
  val revcons : 'a -> 'a list -> 'a list
  val unfoldr : ('a -> bool) -> ('a -> 'b) -> ('a -> 'a) -> 'a -> 'b list
  val make : int -> (int -> 'a) -> 'a list
  val repeat : int -> 'a -> 'a list
  val trappend : 'a list -> 'a list -> 'a list
  val prepend : 'a list -> 'a list -> 'a list
  val postpend : 'a list -> 'a list -> 'a list
  val scanl : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a list
  val eachpair : ('a -> 'a -> 'b) -> 'a list -> 'b list
  val permutations : 'a list -> 'a list list
  val upto : int -> int -> int list
  val ( -- ) : int -> int -> int list
  val random : ?size:(unit -> int) -> (unit -> 'a) -> unit -> 'a list
  val null : 'a list -> bool
  val empty : 'a list -> bool
  val nonempty : 'a list -> bool
  val singleton : 'a list -> bool
  val many : 'a list -> bool
  val prefix : 'a list -> 'a list -> bool
  val foldl : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
  val foldr : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b
  val foldl1 : ('a -> 'a -> 'a) -> 'a list -> 'a
  val foldr1 : ('a -> 'a -> 'a) -> 'a list -> 'a
  val foldl2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
  val foldr2 : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
  val foldwise :
    ?skip:bool -> int -> ('a -> 'b list -> 'a) -> 'a -> 'b list -> 'a
  val conswith : ('a -> 'b) -> 'a -> 'b list -> 'b list
  val conswhen : ('a -> bool) -> 'a -> 'a list -> 'a list
  val snocwith : ('a -> 'b) -> 'b list -> 'a -> 'b list
  val snocwhen : ('a -> bool) -> 'a list -> 'a -> 'a list
  val anded : bool list -> bool
  val ored : bool list -> bool
  val conjunction : ('a -> bool) list -> 'a -> bool
  val disjunction : ('a -> bool) list -> 'a -> bool
  val all : ('a -> bool) -> 'a list -> bool
  val any : ('a -> bool) -> 'a list -> bool
  val sum : int list -> int
  val maximumBy : ?compare:('a -> 'a -> int) -> 'a list -> 'a
  val maximum : 'a list -> 'a
  val minimum : ?compare:('a -> 'a -> int) -> 'a list -> 'a
  val break : ('a -> 'a -> bool) -> 'a list -> 'a list list
  val flatmap : ('a -> 'b list) -> 'a list -> 'b list
  val delete : ?eq:('a -> 'a -> bool) -> 'a -> 'a list -> 'a list
  val replace : 'a -> 'a -> 'a list -> 'a list
  val behead : 'a list -> 'a list -> 'a list
  val prefixes : 'a list -> 'a list list
  val suffixes : 'a list -> 'a list list
  val intersperse : 'a -> 'a list -> 'a list
  val pad : ?left:bool -> def:'a -> int -> 'a list -> 'a list
  val transpose : ?def:'a -> 'a list list -> 'a list list
  val evens : 'a list -> 'a list
  val odds : 'a list -> 'a list
  val splitat : int -> 'a list -> 'a list * 'a list
  val everyother : 'a list -> 'a list
  val take : int -> 'a list -> 'a list
  val drop : int -> 'a list -> 'a list
  val takeall : int -> 'a list -> 'a list list
  val splitwhile : ('a -> bool) -> 'a list -> 'a list * 'a list
  val takewhile : ('a -> bool) -> 'a list -> 'a list
  val dropwhile : ('a -> bool) -> 'a list -> 'a list
  val one_of : 'a list -> 'a -> bool
  val zip : 'a list -> 'b list -> ('a * 'b) list
  val zipwith : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val unzip : ('a * 'b) list -> 'a list * 'b list
  module Assoc = Prelude.Lists.Assoc
  val sorted : ('a -> 'a -> int) -> 'a list -> bool
  val uniq : ?compare:('a -> 'a -> int) -> 'a list -> 'a list
  val uniqc : ?compare:('a -> 'a -> int) -> 'a list -> (int * 'a) list
  val index : ?z:int -> 'a list -> (int * 'a) list
  val pos : ?eq:('a -> 'a -> bool) -> 'a -> 'a list -> int
  val project : ?relaxed:bool -> int list -> 'a list -> 'a list
  val nub : ?compare:('a -> 'a -> int) -> 'a list -> 'a list
  val nub2 : ?compare:('a -> 'a -> int) -> 'a list -> 'a list
  val union : ?compare:('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
  val intersect :
    ?compare:('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
  val subset : ?compare:('a -> 'a -> int) -> 'a list -> 'a list -> bool
  val diff : ?compare:('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
  val cartesian_product : 'a list list -> 'a list list
  val powerset : 'a list -> 'a list list
  val combinations : int -> 'a list -> 'a list list
end

module type ETUDE = sig
  include Endofunctors_intf.Monad.AUGMENTED
  include Traverse_intf.Traversable.List.AUGMENTED
          with type 'a t := 'a t
end

module type AUGMENTED = sig
  include STDLIB
  include PRELUDE with type 'a t := 'a t
  include ETUDE with type 'a t := 'a t
end
