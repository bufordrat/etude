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

    (** [let+] is [flip map] for use in letops syntax.  (For more info
       on letops, please see {{:
       http://jobjo.github.io//2019/04/24/ocaml-has-some-new-shiny-syntax.html}Joel
       Bjornsson's blog post}.) *)
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
    (** Example usage for options, lists, and results:
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
    val ( <&> ) : 'a t -> ('a -> 'b) -> 'b t
    val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
    val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
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

       Note that [pure] is always synonymous with monadic [return]. *)

    val product : 'a t -> 'b t -> ('a * 'b) t

  end

  module type AUGMENTED = sig
    type 'a t

    (** @inline *)
    include BASIC with type 'a t := 'a t

    (** @inline *)
    include Functor.AUGMENTED with type 'a t := 'a t
    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
    val apply : ('a -> 'b) t -> 'a t -> 'b t
    val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
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

     Example usage for options, lists, and results:
{v
# let open Etude.Option in
let k x = if x > 2 then Some x else None in
let mx = Some 12 in
bind mx k;;
- : int option = Etude.Option.Some 12
# let open Etude.Option in
let k x = if x > 2 then Some x else None in
let mx = Some 1 in
bind mx k;;
- : int option = Etude.Option.None
v}

{v
# let open Etude.List in
let pair_up x = [x ; x * 10] in
bind [1;2;3] pair_up;;
- : int list = [1; 10; 2; 20; 3; 30]
# let open Etude.List in
let is_it_even n = if n mod 2 = 0 then [n] else [] in
bind [1;2;3;4;5] is_it_even;;
- : int list = [2; 4]
v}

{v
# let open Etude.Result.Make (String) in
let k x = if x > 2 then Ok x else Error "too small" in
let mx = Ok 12 in
bind mx k;;
- : (int, string) result = Ok 12
# let open Etude.Result.Make (String) in
let k x = if x > 2 then Ok x else Error "too small" in
let mx = Ok 0 in
bind mx k;;
- : (int, string) result = Error "too small"
v}

*)
  end

  module type AUGMENTED = sig
    type 'a t

    (** @inline *)
    include BASIC with type 'a t := 'a t

    (** @inline *)
    include Applicative.AUGMENTED with type 'a t := 'a t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( >=> ) : ('a -> 'b t) -> ('b -> 'c t) -> 'a -> 'c t
    val ( <=< ) : ('a -> 'b t) -> ('c -> 'a t) -> 'c -> 'b t
    val ( >> ) : 'a t -> 'b t -> 'b t
    val join : 'a t t -> 'a t
  end

  module type MAKE = functor (M : BASIC) ->
                     AUGMENTED with type 'a t = 'a M.t
end
module type MONAD = Monad.AUGMENTED
