module Foldable = struct
  module type BASIC = sig
    type 'a t

    include Monoid_intf.MONOID with type 'a t := 'a t

    val foldl : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  end

  module type AUGMENTED = sig
    type 'a t

    include BASIC with type 'a t := 'a t

    val null : 'a t -> bool
  end
end

module type FOLDABLE = Foldable.BASIC

module Traversable = struct
  open Endofunctors_intf

  module type STREAM = sig
    type 'a t

    include Functor.BASIC with type 'a t := 'a t
    include Foldable.BASIC with type 'a t := 'a t
  end

  module type IDIOM = sig
    type 'a t

    include Applicative.BASIC with type 'a t := 'a t
  end

  (* TODO: abstract over container datatypes other than
     lists; Haskell handwaves this a bit *)

  module List = struct
    module type BASIC = sig
      type 'a t

      val sequence : 'a t list -> 'a list t
      (** [sequence] converts a list of applicative values into an
         applicative list by re-consing the list back together under the
         applicative.  For more information on applicative functors in
         OCaml, please see {{:
         https://www.cl.cam.ac.uk/teaching/1617/L28/monads.pdf}these
         course notes}, section 10.4.

         Example usage for options, lists, and results:
{v
# let open Etude.Option in
sequence [Some 1; Some 2; Some 3];;
- : int list option = Etude.Option.Some [1; 2; 3]
# let open Etude.Option in
sequence [Some 1; None; Some 3];;
- : int list option = Etude.Option.None
# let open Etude.Option in
sequence [None; None; None];;
- : 'a list option = Etude.Option.None
v}

{v
# let open Etude.List in
sequence [[1; 2]; [3; 4]];;
- : int list list = [[1; 3]; [1; 4]; [2; 3]; [2; 4]]
# let open Etude.List in
sequence [[1]; [2; 3; 4]; [5]];;
- : int list list = [[1; 2; 5]; [1; 3; 5]; [1; 4; 5]]
# let open Etude.List in
sequence [[1]; [2; 3; 4]; []];;
- : int list list = []
v}

{v
# let open Etude.Result.Make (String) in
sequence [Ok 1; Ok 2; Ok 3];;
- : (int list, string) result = Ok [1; 2; 3]
# let open Etude.Result.Make (String) in
sequence [Ok 1; Error "an error"; Ok 3];;
- : (int list, string) result = Error "an error"
# let open Etude.Result.Make (String) in
sequence [Ok 1; Error "an error"; Error "another error"];;
- : (int list, string) result = Error "an error"
v}
       *)
    end

    module type AUGMENTED = sig
      type 'a t

      (** @inline *)
      include BASIC with type 'a t := 'a t

      val traverse : ('a -> 'b t) -> 'a list -> 'b list t
      (** [traverse f lst] is [sequence (map f lst)].

      Example usage for options, lists, and results:
{v
# let open Etude.Option in
let only_even n = if n mod 2 = 0 then Some n else None in
traverse only_even [2;4;6;8];;
- : int list option = Etude.Option.Some [2; 4; 6; 8]
# let open Etude.Option in
let only_even n = if n mod 2 = 0 then Some n else None in
traverse only_even [1;2;3;4];;
- : int list option = Etude.Option.None
v}

{v
# let open Etude.List in
let double_up n = [n ; n * 10] in
traverse double_up [1;2;3];;
- : int list list =
[[1; 2; 3]; [1; 2; 30]; [1; 20; 3]; [1; 20; 30]; [10; 2; 3]; [10; 2; 30];
 [10; 20; 3]; [10; 20; 30]]
# let open Etude.List in
let only_even n = if n mod 2 = 0 then [n] else [] in
traverse only_even [2;4;6;8];;
- : int list list = [[2; 4; 6; 8]]
# let open Etude.List in
let only_even n = if n mod 2 = 0 then [n] else [] in
traverse only_even [1;2;3];;
- : int list list = []
v}

{v
utop[10]> let open Etude.Result.Make (String) in
let only_even n = if n mod 2 = 0 then Ok n else Error "must be even" in
traverse only_even [2;4;6;8];;
- : (int list, string) result = Ok [2; 4; 6; 8]
utop[11]> let open Etude.Result.Make (String) in
let only_even n = if n mod 2 = 0 then Ok n else Error "must be even" in
traverse only_even [1;2;3;4];;
- : (int list, string) result = Error "must be even"
v}
      *)

      val forM : 'a list -> ('a -> 'b t) -> 'b list t
      (** [forM] is [flip traverse].

          For example usage, see {!traverse}.
      *)
    end

    module type MAKE = functor (I : IDIOM) -> sig
      include AUGMENTED with type 'a t := 'a I.t
    end
  end
end
