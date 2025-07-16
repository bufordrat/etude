module List : sig
  type 'a t = 'a list

  module Make : Traverse_intf.Traversable.List.MAKE
end
