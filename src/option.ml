include Prelude.Option
include Stdlib.Option

open Endofunctors

module OptionMonad = struct
  type 'a t = 'a Stdlib.Option.t
  let pure = some
  let bind = bind
end

module OptionMonoid = struct
  type 'a t = 'a Stdlib.Option.t
  let append ox oy =
    match ox, oy with
    | Some s1, _ -> Some s1
    | _, Some s2 -> Some s2
    | None, None -> None
  let empty = None
end

module M = Monad.Make (OptionMonad)
include M

type 'a t = 'a option = None | Some of 'a

module Monoid = struct
  module Mo = Monoid.Make (OptionMonoid)
  let empty = Mo.empty
  let (<|>) = Mo.(<|>)
  let append = Mo.append
  let asum = Mo.asum
end
include Monoid

let cat_options lst =
  let rec cat_options' acc = function
    | [] -> acc
    | Some x :: xs -> cat_options' (x :: acc) xs
    | None :: xs -> cat_options' acc xs
  in
  List.rev @@ cat_options' [] lst
