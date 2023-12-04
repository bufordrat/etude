module Functor = struct
  module type BASIC = sig
    type 'a t

    val map : ('a -> 'b) -> 'a t -> 'b t
    (** [map] is a generalization of [Stdlib.List.map] to any datatype
       that supports the functor interface, in the endofunctor sense
       of the term—not to be confused with OCaml and SML's module
       functors, which is a different meaning of the word "functor".
       For an introductory explanation of the functor interface, please 
       see {{: https://typeclasses.com/functortown/functor-bifunctor}
       Functortown}.

       Example usage for options, lists, and results:
{v
# let open Etude.Option in
map succ (Some 12);;
- : int option = Etude.Option.Some 13
v}

{v
# let open Etude.List in
map succ [1;2;3];;
- : int list = [2; 3; 4]
v}

{v
# let open Etude.Result.Make (String) in
map succ (Ok 12);;
- : (int, string) result = Ok 13
v}
     *)
  end

  module type AUGMENTED = sig
    type 'a t

    (** @inline *)
    include BASIC with type 'a t := 'a t


    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
    (** [(let+)] is [flip map] for use in {{:
       http://jobjo.github.io//2019/04/24/ocaml-has-some-new-shiny-syntax.html}letops
       syntax}.

       Example usage for options, lists, and results:
{v
# let open Etude.Option in
let+ n = Some 12 in succ n;;
- : int option = Etude.Option.Some 13
v}

{v
# let open Etude.List in
let+ each_element = [1;2;3] in succ each_element;;
- : int list = [2; 3; 4]
v}

{v
# let open Etude.Result.Make (String) in
let+ n = Ok 12 in succ n;;
- : (int, string) result = Ok 13
v}
     *)

    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
    (** [(>>|)] is an infix version of[flip map].

     Example usage for options, lists, and results:

{v
# let open Etude.Option in Some 15 >>| succ;;
- : int option = Etude.Option.Some 16
# let open Etude.Option in None >>| succ;;
- : int option = Etude.Option.None
v}

{v
# let open Etude.List in [1;2;3] >>| succ;;
- : int list = [2; 3; 4]
v}

{v
# let open Etude.Result.Make (String) in Ok 13 >>| succ;;
- : (int, string) result = Ok 14
# let open Etude.Result.Make (String) in Error "oops" >>| succ;;
- : (int, string) result = Error "oops"
v}

     *)

    val ( <&> ) : 'a t -> ('a -> 'b) -> 'b t
    (** [(<&>)] is synonymous with {!(>>|)} and {!(>|=)}. *)

    val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
    (** [(>|=)] is synonymous with {!(>>|)} and {!(<&>)}. *)

    val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
    (** [(<$>)] is an infix version of {!map}. 

        Example usage for options, lists, and results:
{v
# let open Etude.Option in succ <$> Some 13;;
- : int option = Etude.Option.Some 14
# let open Etude.Option in succ <$> None;;
- : int option = Etude.Option.None
v}
     *)
  end

  module type MAKE = functor (F : BASIC) ->
                     AUGMENTED with type 'a t = 'a F.t
end
module type FUNCTOR = Functor.AUGMENTED
               
module Applicative = struct
  module type BASIC = sig
    type 'a t
    include Functor.BASIC with type 'a t := 'a t

    val pure : 'a -> 'a t
    (** [pure] lifts a value of type ['a] into an applicative value
       of type ['a t].  In the option applicative, it is 
       [fun x -> Some x], in the list applicative, it is
       [fun x -> [x]], and in the result applicative it is
       [fun x -> Ok x].
       
       Note that [pure] is synonymous with monadic [return]. *)

    val product : 'a t -> 'b t -> ('a * 'b) t
    (** [product] is the applicative product function.  OCaml uses the
       pure/map/product definition of an applicative functor, as opposed
       to the more familiar pure/apply definition of an applicative
       functor from Haskell.  For more information on the pure/map/product
       definition, please see {{:
       http://www.staff.city.ac.uk/~ross/papers/Applicative.html}
       McBride and Patterson} (2008), section 7.  [product] is mainly 
       used for applicative {{:
       http://jobjo.github.io//2019/04/24/ocaml-has-some-new-shiny-syntax.html}letops syntax}.

       Example usage for options, lists, and results:
{v
# let open Etude.Option in
product (Some 12) (Some 13);;
- : (int * int) option = Etude.Option.Some (12, 13)
# let open Etude.Option in
product None (Some 13);;
- : ('a * int) option = Etude.Option.None
v}

{v
# let open Etude.List in
product [1;2;3] [4;5;6];;
- : (int * int) list =
[(1, 4); (1, 5); (1, 6); (2, 4); (2, 5); (2, 6); (3, 4); (3, 5); (3, 6)]
v}

{v
# let open Etude.Result.Make (String) in
product (Ok 12) (Ok 13);;
- : (int * int, string) result = Ok (12, 13)
# let open Etude.Result.Make (String) in
product (Ok 12) (Error "not ok!");;
- : (int * 'a, string) result = Error "not ok!"
v}
      *)
  end

  module type AUGMENTED = sig
    type 'a t

    (** @inline *)
    include BASIC with type 'a t := 'a t

    (** @inline *)
    include Functor.AUGMENTED with type 'a t := 'a t

    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
    (** [(and+)] is {!product} for use in {{:
       http://jobjo.github.io//2019/04/24/ocaml-has-some-new-shiny-syntax.html}letops
       syntax}. 

       Example usage for options, lists, and results:
{v
# let open Etude.Option in
let+ x = Some 1 and+ y = Some 2 in x + y;;
- : int option = Etude.Option.Some 3
# let open Etude.Option in
let+ x = None and+ y = Some 2 in x + y;;
- : int option = Etude.Option.None
v}

{v
# let open Etude.List in
let+ x = [1;2] and+ y = [10; 20] in x + y;;
- : int list = [11; 21; 12; 22]
v}

{v
# let open Etude.Result.Make (String) in
let+ x = Ok 1 and+ y = Ok 2 in x + y;;
- : (int, string) result = Ok 3
# let open Etude.Result.Make (String) in
let+ x = Ok 1 and+ y = Error "yikes" in x + y;;
- : (int, string) result = Error "yikes"
v}
     *)

    val apply : ('a -> 'b) t -> 'a t -> 'b t
    (** [apply] provides an embellished version of function application
        under the heading of an {{:
        http://www.staff.city.ac.uk/~ross/papers/Applicative.html
        }applicative functor}.

        See {!(<*>)} for example usage.
     *)

    val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
    (** [(<*>)] is an infix version of {!apply}.
    
        Example usage for options, lists, and results:
{v
# let open Etude.Option in
pure (+) <*> Some 1 <*> Some 2;;
- : int option = Etude.Option.Some 3
# let open Etude.Option in
pure (+) <*> None <*> Some 2;;
- : int option = Etude.Option.None
v}

{v
utop[14]> let open List in
pure (fun x y -> x,y) <*> [1;2;3] <*> [4;5;6];;
- : (int * int) list =
[(1, 4); (1, 5); (1, 6); (2, 4); (2, 5); (2, 6); (3, 4); (3, 5); (3, 6)]
v}

{v
# let open Etude.Result.Make (String) in
pure (+) <*> Ok 1 <*> Ok 2;;
- : (int, string) result = Ok 3
# let open Etude.Result.Make (String) in
pure (+) <*> Ok 1 <*> Error "whoops" ;;
- : (int, string) result = Error "whoops"
v}
     *)
  end

  module type MAKE = functor (A : BASIC) ->
                     AUGMENTED with type 'a t = 'a A.t 
end
module type APPLICATIVE = Applicative.AUGMENTED

module Monad = struct
  module type BASIC = sig
    type 'a t
    val pure : 'a -> 'a t

    val bind : 'a t -> ('a -> 'b t) -> 'b t
  (** [bind] is monadic bind.  Bind takes a monadic value [mx] of type
     ['a t] and a Kleisli arrow function [k] of type ['a -> 'b t], and
     allows you to pipe [mx] into [k], despite the fact that the input
     type of [k] is ['a] and not ['a t].  It can be thought of as
     monadic piping.  For more information on monadic style in OCaml,
     please see {{:
     https://cs3110.github.io/textbook/chapters/ds/monads.html}these
     course notes}.

     See the guide to {!page-endofunctors.bind} for example usage.
*)
  end

  module type AUGMENTED = sig
    type 'a t

    (** @inline *)
    include BASIC with type 'a t := 'a t

    (** @inline *)
    include Applicative.AUGMENTED with type 'a t := 'a t

    val return : 'a -> 'a t
    (** [return] is {!pure}.  *)

    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    (** [(>>=)] is an infix version of {!bind}.

    Example usage for options, lists, and results:
{v
# let open Etude.Option in
let k x = if x > 2 then Some x else None in
Some 12 >>= k;;
- : int option = Etude.Option.Some 12
# let open Etude.Option in
let k x = if x > 2 then Some x else None in
Some 1 >>= k;;
- : int option = Etude.Option.None
v}

{v
# let open Etude.List in
let pair_up x = [x ; x * 10] in
[1; 2; 3] >>= pair_up;;
- : int list = [1; 10; 2; 20; 3; 30]
# let open Etude.List in
let is_it_even n = if n mod 2 = 0 then [n] else [] in
[1; 2; 3; 4; 5] >>= is_it_even;;
- : int list = [2; 4]
v}

{v
# let open Etude.Result.Make (String) in
let k x = if x > 2 then Ok x else Error "too small" in
Ok 12 >>= k;;
- : (int, string) result = Ok 12
# let open Etude.Result.Make (String) in
let k x = if x > 2 then Ok x else Error "too small" in
Ok 0 >>= k;;
- : (int, string) result = Error "too small"
v}

*)

    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    (** [( let* )] is {!bind} for use in {{:
       http://jobjo.github.io//2019/04/24/ocaml-has-some-new-shiny-syntax.html}letops
       syntax}. 

       Example usage for options, lists, and results:
{v
# let open Etude.Option in 
let k x = if x > 2 then Some x else None in
let* x = Some 13 in
k x;;
- : int option = Etude.Option.Some 13
# let open Etude.Option in 
let k x = if x > 2 then Some x else None in
let* x = Some (-2) in
k x;;
- : int option = Etude.Option.None
v}

{v
# let open Etude.List in
let pair_up x = [x ; x * 10] in
let* each_item = [1; 2; 3] in
pair_up each_item;;
- : int list = [1; 10; 2; 20; 3; 30]
# let open Etude.List in 
let is_it_even n = if n mod 2 = 0 then [n] else [] in
let* each_item = [1; 2; 3; 4; 5] in
is_it_even each_item;;
- : int list = [2; 4]
v}

{v
# let open Etude.Result.Make (String) in
let k x = if x > 2 then Ok x else Error "too small" in
let* x = Ok 13 in
k x;;
- : (int, string) result = Ok 13
# let open Etude.Result.Make (String) in
let k x = if x > 2 then Ok x else Error "too small" in
let* x = Ok 1 in
k x;;
- : (int, string) result = Error "too small"
v}
     *)

    val ( >=> ) : ('a -> 'b t) -> ('b -> 'c t) -> 'a -> 'c t
    (** [(>=>)] is Kleisli pre-composition.  It provides an embellished
       version of function pre-composition under a monad, similarly to
       how {!bind} provides an embellished version of piping under a
       monad.  
       
       Example usage for options, results, and lists: 
{v
# let open Etude.Option in
let k1 n = if n mod 2 = 0 then Some n else None in
let k2 n = if n < 5 then Some n else None in
let* x = Some 2 in
(k1 >=> k2) x;;
- : int option = Etude.Option.Some 2
# let open Etude.Option in
let k1 n = if n mod 2 = 0 then Some n else None in
let k2 n = if n < 5 then Some n else None in
let* x = Some 3 in
(k1 >=> k2) x;;
v}

{v
# let open Etude.List in 
let is_it_even n = if n mod 2 = 0 then [n] else [] in
let pair_up n = [n ; n * 10] in
let* x = [1; 2; 3] in
(is_it_even >=> pair_up) x;;
- : int list = [2; 20]
# let open Etude.List in 
let is_it_even n = if n mod 2 = 0 then [n] else [] in
let pair_up n = [n ; n * 10] in
let* x = [1; 2; 3] in
(pair_up >=> is_it_even) x;;
- : int list = [10; 2; 20; 30]
v}

{v
# let open Etude.Result.Make (String) in
let k1 n = if n mod 2 = 0 then Ok n else Error "should be even" in
let k2 n = if n < 5 then Ok n else Error "too small" in
let* x = Ok 11 in
(k1 >=> k2) x;;
- : (int, string) result = Error "should be even"
# let open Etude.Result.Make (String) in
let k1 n = if n mod 2 = 0 then Ok n else Error "should be even" in
let k2 n = if n < 5 then Ok n else Error "too small" in
let* x = Ok 11 in
(k2 >=> k1) x;;
- : (int, string) result = Error "too small"
v}
     *)
      
    val ( <=< ) : ('a -> 'b t) -> ('c -> 'a t) -> 'c -> 'b t
    (** [(<=<)] is Kleisli composition, i.e. the flipped version of
       {!(>=>)}.

     For example usage, see {!(>=>)}. *)

    val ( >> ) : 'a t -> 'b t -> 'b t
    (** [(>>)] is a monadic sequential checking function.  [mx >> my]
       performs whatever simulated monadic effect is associated with
       [mx], discards the data associated with [mx], performs whatever
       simulated monadic effect is associated with [my], and
       ultimately evaluates to [my].

       [mx >> my] is equivalent to [let* _ = mx in my].

       Example usage for options, lists, and results: 

{v
# let open Etude.Option in
Some 1 >> Some 2;;
- : int option = Etude.Option.Some 2
# let open Etude.Option in
Some 1 >> None;;
- : 'a option = Etude.Option.None
v}

{v
# let open Etude.List in
[1;2;3] >> [1;2;3];;
- : int list = [1; 2; 3; 1; 2; 3; 1; 2; 3]
# let open Etude.List in
[] >> [1;2;3];;
- : int list = []
# let open List in
[1;2;3] >> [];;
- : 'a list = []
v}

{v
# let open Etude.Result.Make (String) in
Ok 1 >> Ok 2;;
- : (int, string) result = Ok 2
# let open Etude.Result.Make (String) in
Ok 1 >> Error "bad second value";;
- : ('a, string) result = Error "bad second value"
# let open Etude.Result.Make (String) in
Error "bad first value" >> Error "bad second value";;
- : ('a, string) result = Error "bad first value"
v}
     *)

    val join : 'a t t -> 'a t
    (** [join] is monadic join.  It converts a doubly-monadic value into
       a single-monadic value.  In the list monad, [join] is synonymous 
       with {!List.concat} and {!List.flatten}.

       Note that in all monads, {!bind} is equivalent to 
       [fun mx k -> join (map k mx)].

       Example usage for options, results, and lists:
{v
# let open Etude.Option in
join (Some (Some 7));;
- : int option = Etude.Option.Some 7
# let open Etude.Option in
join (Some None);;
- : 'a option = Etude.Option.None
# let open Etude.Option in
join None;;
- : 'a option = Etude.Option.None
v}

{v
# let open Etude.List in
join [[1; 2; 3]; [4; 5; 6]];;
- : int list = [1; 2; 3; 4; 5; 6]
# let open Etude.List in
join [[]; []; []];;
- : 'a list = []
v}

{v
# let open Etude.Result.Make (String) in
join (Ok (Ok 4));;
- : (int, string) result = Ok 4
# let open Etude.Result.Make (String) in
join (Ok (Error "uh-oh"));;
- : ('a, string) result = Error "uh-oh"
# let open Etude.Result.Make (String) in
join (Error "uh-oh");;
- : ('a, string) result = Error "uh-oh"
v}
     *)

  end

  module type MAKE = functor (M : BASIC) ->
                     AUGMENTED with type 'a t = 'a M.t
end
module type MONAD = Monad.AUGMENTED
