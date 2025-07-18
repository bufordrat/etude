module type ERROR = sig
  type t
end

module type AUGMENTED = Result_intf.AUGMENTED
module type MAKE = Result_intf.MAKE

module Make : MAKE =
functor
  (E : ERROR)
  ->
  struct
    open Endofunctors
    include Prelude.Result
    include Stdlib.Result

    module ResultMonad = struct
      type 'a t = ('a, E.t) result

      let pure = Stdlib.Result.ok
      let bind = Stdlib.Result.bind
    end

    module M = Monad.Make (ResultMonad)
    include M

    module Traverse = struct
      module T = Traverse.List.Make (M)

      let sequence = T.sequence
      let forM = T.forM
      let traverse = T.traverse
    end

    include Traverse

    (* non-functorized/handwritten stuff goes here *)

    let bind_error mx k =
      match mx with
      | Error e -> k e
      | Ok o -> Ok o

    let oks lst =
      let open Prelude.List in
      let reducer output = function
        | Ok o -> o :: output
        | Error _ -> output
      in
      foldl reducer [] lst |> rev

    let errors lst =
      let open Prelude.List in
      let reducer output = function
        | Ok _ -> output
        | Error e -> e :: output
      in
      foldl reducer [] lst |> rev
  end
