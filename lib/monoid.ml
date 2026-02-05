module type BASIC = Monoid_intf.Monoid.BASIC
module type AUGMENTED = Monoid_intf.Monoid.AUGMENTED

module Make (M : BASIC) :
  AUGMENTED with type 'a t := 'a M.t = struct
  open Prelude.List

  let append = M.append
  let empty = M.empty
  let ( <|> ) = M.append
  let asum ms = foldl ( <|> ) empty ms
end
