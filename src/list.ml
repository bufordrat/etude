include Prelude.List
include Stdlib.List

module Assoc = Prelude.Assoc

open Endofunctors

module ListMonad = struct
  type 'a t = 'a Stdlib.List.t
  let pure x = [x]
  let bind = Prelude.List.bind
end

module ListMonoid = struct
  type 'a t = 'a Stdlib.List.t
  let empty = []
  let append = (@)
end

module M = Monad.Make (ListMonad)
include M

module Traverse = struct
  module T = Traverse.List.Make (M)
  let sequence = T.sequence
  let forM = T.forM
  let traverse = T.traverse
end
include Traverse

module Monoid = struct
  module Mo = Monoid.Make (ListMonoid)
  let append = Mo.append
  let empty = Mo.empty
  let (<|>) = Mo.(<|>)
  let asum = Stdlib.List.concat
end
include Monoid
