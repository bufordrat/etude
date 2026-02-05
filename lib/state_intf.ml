module type PURESTATE = sig
  type t
end

module type BASIC = sig
  type ('state, 'a) t

  val put : 'state -> ('state, unit) t
  val get : ('state, 'state) t
  val eval : ('state, 'a) t -> 'state -> 'a
  val exec : ('state, 'a) t -> 'state -> 'state
  val run : ('state, 'a) t -> 'state -> 'a * 'state
end

module type MAKE = functor (S : PURESTATE) -> sig
  include BASIC

  include
    Endofunctors_intf.Monad.AUGMENTED
      with type 'a t := (S.t, 'a) t
end
