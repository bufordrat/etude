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

    (** @inline *)
    include Semigroup.BASIC with type 'a t := 'a t
    val empty : 'a t
  end

  module type AUGMENTED = sig
    type 'a t

    (** @inline *)
    include Semigroup.AUGMENTED with type 'a t := 'a t

    (** @inline *)
    include BASIC with type 'a t := 'a t

    (* TODO: abstract the list part over foldable container types *)
          
    val asum : 'a t list -> 'a t
  end
end
module type MONOID = Monoid.AUGMENTED

