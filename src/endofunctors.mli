module Functor : sig
  module type BASIC =
    Endofunctors_intf.Functor.BASIC

  module type AUGMENTED =
    Endofunctors_intf.Functor.AUGMENTED

  module Make : functor (F : BASIC) ->
                AUGMENTED with type 'a t = 'a F.t
end

module Applicative : sig
  module type BASIC =
    Endofunctors_intf.Applicative.BASIC

  module type AUGMENTED =
    Endofunctors_intf.Applicative.AUGMENTED

  module Make : functor (A : BASIC) ->
                AUGMENTED with type 'a t = 'a A.t 
end

module Monad : sig
  module type BASIC =
    Endofunctors_intf.Monad.BASIC

  module type AUGMENTED =
    Endofunctors_intf.Monad.AUGMENTED

  module Make : functor (M : BASIC) ->
                AUGMENTED with type 'a t = 'a M.t
end
