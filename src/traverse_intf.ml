module Foldable = struct
  module type BASIC = sig
    type 'a t
    include Monoid_intf.MONOID with type 'a t := 'a t
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
  open Endofunctors_intf

  module type STREAM = sig
    type 'a t
    include Functor.BASIC with type 'a t := 'a t
    include Foldable.BASIC with type 'a t := 'a t
  end
  
  module type IDIOM = sig
    type 'a t
    include Applicative.BASIC with type 'a t := 'a t
  end

  (* TODO: abstract over container datatypes other than lists; Haskell
     handwaves this a bit *)

  module List = struct
    module type BASIC = sig
      type 'a t 
      val sequence : 'a t list -> 'a list t
    end

    module type AUGMENTED = sig
      type 'a t
         
      (** @inline *)
      include BASIC with type 'a t := 'a t
      val traverse : ('a -> 'b t) -> 'a list -> 'b list t
      val forM : 'a list -> ('a -> 'b t) -> 'b list t
    end

    module type MAKE =
      functor (I : IDIOM) -> sig
        include AUGMENTED with type 'a t := 'a I.t 
      end
  end
end
