module Semigroup = struct
  module type BASIC = sig
    type 'a t
    val append : 'a t -> 'a t -> 'a t
  end

  module type AUGMENTED = sig
    type 'a t
    include BASIC with type 'a t := 'a t
    val (<|>) : 'a t -> 'a t -> 'a t
  end
end
module type SEMIGROUP = Semigroup.BASIC

module Monoid = struct
  module type BASIC = sig
    type 'a t
    include Semigroup.BASIC with type 'a t := 'a t
    val empty : 'a t
  end

  module type AUGMENTED = sig
    type 'a t
    include Semigroup.AUGMENTED with type 'a t := 'a t
    include BASIC with type 'a t := 'a t
    val sum : 'a t list -> 'a t
  end
end
module type MONOID = Monoid.BASIC

module Foldable = struct
  module type BASIC = sig
    type 'a t
    include MONOID with type 'a t := 'a t
    val foldl : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  end

  module type AUGMENTED = sig
    type 'a t
    include BASIC with type 'a t := 'a t
    val null : 'a t -> bool
  end
end
module type FOLDABLE = Foldable.BASIC

module Traversable = struct
  module type BASIC = sig
    open Endofunctors_intf

    type 'a stream
    include Functor.BASIC with type 'a t := 'a stream
    include Foldable.BASIC with type 'a t := 'a stream

    type 'a idiom
    include Applicative.BASIC with type 'a t := 'a idiom
  end
end
module type TRAVERSABLE = Traversable.BASIC
