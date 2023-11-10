module Functor : sig
  module type BASIC =
    Endofunctors_intf.Functor.BASIC

  module type AUGMENTED =
    Endofunctors_intf.Functor.AUGMENTED

  module Make : Endofunctors_intf.Functor.MAKE
end

module Applicative : sig
  module type BASIC =
    Endofunctors_intf.Applicative.BASIC

  module type AUGMENTED =
    Endofunctors_intf.Applicative.AUGMENTED

  module Make : Endofunctors_intf.Applicative.MAKE
end

module Monad : sig
  module type BASIC =
    Endofunctors_intf.Monad.BASIC

  module type AUGMENTED =
    Endofunctors_intf.Monad.AUGMENTED

  module Make : Endofunctors_intf.Monad.MAKE
end
