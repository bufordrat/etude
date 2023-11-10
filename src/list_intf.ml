(* kind of klugey, but I re-created my own STDLIB list signature (with
   Merlin's help) in order to avoid that toplevel display bug; this is
   Stdlib as of compiler version 4.14: *)

(* utop[3]> open Stdlib.List;;
 * utop[4]> [1;2;3];;
 * - : int list = (::) (1, [2; 3]) *)

module type STDLIB = sig
  type 'a t
  val length : 'a t -> int
  val compare_lengths : 'a t -> 'b t -> int
  val compare_length_with : 'a t -> int -> int
  val cons : 'a -> 'a t -> 'a t
  val hd : 'a t -> 'a
  val tl : 'a t -> 'a t
  val nth : 'a t -> int -> 'a
  val nth_opt : 'a t -> int -> 'a option
  val rev : 'a t -> 'a t
  val init : int -> (int -> 'a) -> 'a t
  val append : 'a t -> 'a t -> 'a t
  val rev_append : 'a t -> 'a t -> 'a t
  val concat : 'a t t -> 'a t
  val flatten : 'a t t -> 'a t
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val iter : ('a -> unit) -> 'a t -> unit
  val iteri : (int -> 'a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
  val rev_map : ('a -> 'b) -> 'a t -> 'b t
  val filter_map : ('a -> 'b option) -> 'a t -> 'b t
  val concat_map : ('a -> 'b t) -> 'a t -> 'b t
  val fold_left_map : ('a -> 'b -> 'a * 'c) -> 'a -> 'b t -> 'a * 'c t
  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val rev_map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b t -> 'c t -> 'a
  val fold_right2 : ('a -> 'b -> 'c -> 'c) -> 'a t -> 'b t -> 'c -> 'c
  val for_all : ('a -> bool) -> 'a t -> bool
  val exists : ('a -> bool) -> 'a t -> bool
  val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
  val exists2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
  val mem : 'a -> 'a t -> bool
  val memq : 'a -> 'a t -> bool
  val find : ('a -> bool) -> 'a t -> 'a
  val find_opt : ('a -> bool) -> 'a t -> 'a option
  val find_map : ('a -> 'b option) -> 'a t -> 'b option
  val filter : ('a -> bool) -> 'a t -> 'a t
  val find_all : ('a -> bool) -> 'a t -> 'a t
  val filteri : (int -> 'a -> bool) -> 'a t -> 'a t
  val partition : ('a -> bool) -> 'a t -> 'a t * 'a t
  val partition_map :
    ('a -> ('b, 'c) Either.t) -> 'a t -> 'b t * 'c t
  val assoc : 'a -> ('a * 'b) t -> 'b
  val assoc_opt : 'a -> ('a * 'b) t -> 'b option
  val assq : 'a -> ('a * 'b) t -> 'b
  val assq_opt : 'a -> ('a * 'b) t -> 'b option
  val mem_assoc : 'a -> ('a * 'b) t -> bool
  val mem_assq : 'a -> ('a * 'b) t -> bool
  val remove_assoc : 'a -> ('a * 'b) t -> ('a * 'b) t
  val remove_assq : 'a -> ('a * 'b) t -> ('a * 'b) t
  val split : ('a * 'b) t -> 'a t * 'b t
  val combine : 'a t -> 'b t -> ('a * 'b) t
  val sort : ('a -> 'a -> int) -> 'a t -> 'a t
  val stable_sort : ('a -> 'a -> int) -> 'a t -> 'a t
  val fast_sort : ('a -> 'a -> int) -> 'a t -> 'a t
  val sort_uniq : ('a -> 'a -> int) -> 'a t -> 'a t
  val merge : ('a -> 'a -> int) -> 'a t -> 'a t -> 'a t
  val to_seq : 'a t -> 'a Seq.t
  val of_seq : 'a Seq.t -> 'a t
end

module type PRELUDE = sig
  type 'a t
  val len : 'a t -> int
  val comparison : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val to_string :
    ?left:string ->
    ?sep:string -> ?right:string -> ('a -> string) -> 'a t -> string
  val head : 'a t -> 'a option
  val tail : 'a t -> 'a t option
  val last : 'a t -> 'a
  val get : int -> 'a t -> 'a
  val snoc : 'a t -> 'a -> 'a t
  val consup : 'a -> 'a t
  val revcons : 'a -> 'a t -> 'a t
  val unfoldr : ('a -> bool) -> ('a -> 'b) -> ('a -> 'a) -> 'a -> 'b t
  val make : int -> (int -> 'a) -> 'a t
  val repeat : int -> 'a -> 'a t
  val trappend : 'a t -> 'a t -> 'a t
  val prepend : 'a t -> 'a t -> 'a t
  val postpend : 'a t -> 'a t -> 'a t
  val scanl : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a t
  val eachpair : ('a -> 'a -> 'b) -> 'a t -> 'b t
  val permutations : 'a t -> 'a t t
  val upto : int -> int -> int t
  val ( -- ) : int -> int -> int t
  val random : ?size:(unit -> int) -> (unit -> 'a) -> unit -> 'a t
  val null : 'a t -> bool
  val empty : 'a t -> bool
  val nonempty : 'a t -> bool
  val singleton : 'a t -> bool
  val many : 'a t -> bool
  val prefix : 'a t -> 'a t -> bool
  val foldl : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  val foldr : ('a -> 'b -> 'b) -> 'b -> 'a t -> 'b
  val foldl1 : ('a -> 'a -> 'a) -> 'a t -> 'a
  val foldr1 : ('a -> 'a -> 'a) -> 'a t -> 'a
  val foldl2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b t -> 'c t -> 'a
  val foldr2 : ('a -> 'b -> 'c -> 'c) -> 'a t -> 'b t -> 'c -> 'c
  val foldwise :
    ?skip:bool -> int -> ('a -> 'b t -> 'a) -> 'a -> 'b t -> 'a
  val conswith : ('a -> 'b) -> 'a -> 'b t -> 'b t
  val conswhen : ('a -> bool) -> 'a -> 'a t -> 'a t
  val snocwith : ('a -> 'b) -> 'b t -> 'a -> 'b t
  val snocwhen : ('a -> bool) -> 'a t -> 'a -> 'a t
  val anded : bool t -> bool
  val ored : bool t -> bool
  val conjunction : ('a -> bool) t -> 'a -> bool
  val disjunction : ('a -> bool) t -> 'a -> bool
  val all : ('a -> bool) -> 'a t -> bool
  val any : ('a -> bool) -> 'a t -> bool
  val sum : int t -> int
  val maximumBy : ?compare:('a -> 'a -> int) -> 'a t -> 'a
  val maximum : 'a t -> 'a
  val minimum : ?compare:('a -> 'a -> int) -> 'a t -> 'a
  val break : ('a -> 'a -> bool) -> 'a t -> 'a t t
  val flatmap : ('a -> 'b t) -> 'a t -> 'b t
  val delete : ?eq:('a -> 'a -> bool) -> 'a -> 'a t -> 'a t
  val replace : 'a -> 'a -> 'a t -> 'a t
  val behead : 'a t -> 'a t -> 'a t
  val prefixes : 'a t -> 'a t t
  val suffixes : 'a t -> 'a t t
  val intersperse : 'a -> 'a t -> 'a t
  val pad : ?left:bool -> def:'a -> int -> 'a t -> 'a t
  val transpose : ?def:'a -> 'a t t -> 'a t t
  val evens : 'a t -> 'a t
  val odds : 'a t -> 'a t
  val splitat : int -> 'a t -> 'a t * 'a t
  val everyother : 'a t -> 'a t
  val take : int -> 'a t -> 'a t
  val drop : int -> 'a t -> 'a t
  val takeall : int -> 'a t -> 'a t t
  val splitwhile : ('a -> bool) -> 'a t -> 'a t * 'a t
  val takewhile : ('a -> bool) -> 'a t -> 'a t
  val dropwhile : ('a -> bool) -> 'a t -> 'a t
  val one_of : 'a t -> 'a -> bool
  val zip : 'a t -> 'b t -> ('a * 'b) t
  val zipwith : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val unzip : ('a * 'b) t -> 'a t * 'b t
  module Assoc = Prelude.Lists.Assoc
  val sorted : ('a -> 'a -> int) -> 'a t -> bool
  val uniq : ?compare:('a -> 'a -> int) -> 'a t -> 'a t
  val uniqc : ?compare:('a -> 'a -> int) -> 'a t -> (int * 'a) t
  val index : ?z:int -> 'a t -> (int * 'a) t
  val pos : ?eq:('a -> 'a -> bool) -> 'a -> 'a t -> int
  val project : ?relaxed:bool -> int t -> 'a t -> 'a t
  val nub : ?compare:('a -> 'a -> int) -> 'a t -> 'a t
  val nub2 : ?compare:('a -> 'a -> int) -> 'a t -> 'a t
  val union : ?compare:('a -> 'a -> int) -> 'a t -> 'a t -> 'a t
  val intersect :
    ?compare:('a -> 'a -> int) -> 'a t -> 'a t -> 'a t
  val subset : ?compare:('a -> 'a -> int) -> 'a t -> 'a t -> bool
  val diff : ?compare:('a -> 'a -> int) -> 'a t -> 'a t -> 'a t
  val cartesian_product : 'a t t -> 'a t t
  val powerset : 'a t -> 'a t t
  val combinations : int -> 'a t -> 'a t t
end

module type ETUDE = sig
  include Endofunctors_intf.Monad.AUGMENTED
  include Traverse_intf.Traversable.List.AUGMENTED
          with type 'a t := 'a t
end

module type AUGMENTED = sig
  type 'a t = 'a Stdlib.List.t
  include STDLIB with type 'a t := 'a t
  include PRELUDE with type 'a t := 'a t
  include ETUDE with type 'a t := 'a t
end
