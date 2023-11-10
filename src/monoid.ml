module Monoid = struct
  module type BASIC =
    Monoid_intf.Monoid.BASIC

  module type AUGMENTED =
    Monoid_intf.Monoid.AUGMENTED

  module Make (M : BASIC) : AUGMENTED
    = struct
    open Prelude.List
    type 'a t = 'a M.t
    let append = M.append
    let empty = M.empty
    let (<|>) = M.append
    let sum ms = foldl (<|>) empty ms
    end
end
