module Semigroup = struct
  module type BASIC = sig
    type 'a t
    val append : 'a t -> 'a t -> 'a t
    (** [append] is a general-purpose append operation.  Officially, in
       the context of the monoid interface, all "append" means is "an
       associative two-place operation".  For options, it implements
       "try A, then if A fails, try B" logic.  For lists, it is
       [List.(@)].

       For usage examples, please see {!Etude.Monoid.Make.(<|>)}.
     *)

  end

  module type AUGMENTED = sig
    type 'a t
    include BASIC with type 'a t := 'a t
    
    val (<|>) : 'a t -> 'a t -> 'a t
    (** [(<|>)] is {!append}.

       Example usage for options and lists:
{v
# let open Etude.Option in
Some 12 <|> Some 13;;
- : int option = Etude.Option.Some 12
# let open Etude.Option in
None <|> Some 13;;
- : int option = Etude.Option.Some 13
# let open Etude.Option in
None <|> None;;
- : 'a option = Etude.Option.None
# let open Etude.Option in
Some 12 <|> None;;
- : int option = Etude.Option.Some 12
v}

{v
utop[4]> let open Etude.List in
[1;2;3] <|> [4;5;6];;
- : int list = [1; 2; 3; 4; 5; 6]
v}
     *)
  end
end
module type SEMIGROUP = Semigroup.BASIC

module Monoid = struct
  module type BASIC = sig
    type 'a t

    (** @inline *)
    include Semigroup.BASIC with type 'a t := 'a t

    val empty : 'a t
    (** [empty] is the identity element of the monoid, which means that
       for any value [a], [empty <|> a] and [a <|> empty] both equal
       [a]. *)

  end

  module type AUGMENTED = sig
    type 'a t

    (** @inline *)
    include Semigroup.AUGMENTED with type 'a t := 'a t

    (** @inline *)
    include BASIC with type 'a t := 'a t

    (* TODO: abstract the list part over foldable container types *)
          
    val asum : 'a t list -> 'a t
    (** [asum] left folds [(<|>)] over the input list, with [empty] as
       the initial accumulator.

       Example usage for options and lists:
{v
# let open Etude.Option in 
asum [Some 1; Some 2; Some 3; Some 4];;
- : int option = Etude.Option.Some 1
# let open Etude.Option in 
asum [Some 1; None; None; None];;
- : int option = Etude.Option.Some 1
# let open Etude.Option in 
asum [None; None; Some 3; None];;
- : int option = Etude.Option.Some 3
# let open Etude.Option in 
asum [];;
- : 'a option = Etude.Option.None
v}

{v
# let open Etude.List in
asum [[1;2;3];[4;5;6];[7;8;9]];;
- : int list = [1; 2; 3; 4; 5; 6; 7; 8; 9]
v}
       *)
  end
end
module type MONOID = Monoid.AUGMENTED

