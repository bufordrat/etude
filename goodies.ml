open Prelude

module type FUNCTOR = sig
  type 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
end
   
module type APPLICATIVE = sig
  include FUNCTOR
  val product : 'a t -> 'b t -> ('a * 'b) t
end

module type MONAD = sig
  type 'a t
  val pure : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type MONOID = sig
  type 'a t
  val empty : 'a t
  val append : 'a t -> 'a t -> 'a t
end

module type FOLDABLE = sig
  include MONOID
  val foldl : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  val null : 'a t -> bool
end

module type TOKEN = sig
  include FOLDABLE
  type tok
  type stream = tok t
  val pop : 'a t -> 'a option * 'a t
  val cons : 'a -> 'a t -> 'a t
  val re_append : 'a t -> 'a t -> 'a t
  val rev: 'a t -> 'a t
end

module FunctorGoodies (F : FUNCTOR) = struct
  let (let+) x f = F.map f x
  let (>>|) = (let+)
  let (<&>) = (let+)
  let (>|=) = (let+)
  let (<$>) = F.map

  module Compose (F1 : FUNCTOR) (F2 : FUNCTOR) : FUNCTOR = struct
    type 'a t = 'a F1.t F2.t
    let map f composed = F2.map (F1.map f) composed
  end
end
                  
module ApplicativeGoodies (A : APPLICATIVE) = struct
  include FunctorGoodies (A)
  let (and+) = A.product
  let apply af ax =
    A.map
      (fun (f, x) -> f x)
      (A.product af ax)
  let (<*>) = apply
end

module MonadGoodies (M : MONAD) = struct
  let pure = M.pure
  let bind = M.bind
  let (>>=) = bind
  let (let*) = bind
  let (>=>) mf mg x = mf x >>= mg
  let (<=<) mf mg x = mg x >>= mf
  let (>>) mx my = mx >>= fun _ -> my
end
                               
module Monad2App (M : MONAD) = struct
  include MonadGoodies (M)

  (* reduction of applicative interface to monadic interface *)
  let map f mx = let* x = mx in
                 pure (f x)
  let product ax ay = let* x = ax in
                      let* y = ay in
                      pure (x,y)

  module I = struct
    type 'a t = 'a M.t
    let map = map
    let product = product
  end

  include ApplicativeGoodies (I)
  open Fun
  let ( <* ) ax ay = pure const <*> ax <*> ay
  let ( *> ) ax ay = pure (flip const) <*> ax <*> ay
end

(* helper functions for optional values *)
module Option = struct

  (* unwraps the Somes; throws the None-s out *)
  include Stdlib.Option
  include Prelude.Option
  let rec cat_options = function
    | [] -> []
    | Some x :: xs -> x :: cat_options xs
    | None :: xs -> cat_options xs

  (* for auto-generating monad and applicative stuff *)
  module OptionMonad = struct
    type 'a t = 'a option
    let pure = Option.some
    let bind = Option.(>>=)
  end

  include Monad2App (OptionMonad)
end

module type ERROR = sig
  type t
end

module Result = struct
  (* module functor for building a Result module with cool extra stuff
     in it; takes a module containing the error type as an input *) 
  module Make (E : ERROR) = struct
    include Stdlib.Result
    include Prelude.Result
    
    (* for auto-generating monad and applicative stuff *)
    module ResultMonad = struct
      type 'a t = ('a, E.t) result
      let pure = Result.ok
      let bind = Result.(>>=)
    end
    
    include Monad2App (ResultMonad)
  end
end
