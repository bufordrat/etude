module Functor = struct
  module type BASIC =
    Endofunctors_intf.Functor.BASIC

  module type AUGMENTED =
    Endofunctors_intf.Functor.AUGMENTED

  module Make (F : BASIC)
         : AUGMENTED with type 'a t = 'a F.t
    = struct
    include F 
    let ( let+ ) x f = map f x
    let ( >>| ) = ( let+ )
    let ( <&> ) = ( let+ )
    let ( >|= ) = ( let+ )
    let ( <$> ) = map
  end

  module Compose (F1 : BASIC) (F2 : BASIC)
         : BASIC with type 'a t = 'a F2.t F1.t
    = struct
    type 'a t = 'a F2.t F1.t
    let map f composed = F1.map (F2.map f) composed
  end
end
module type FUNCTOR = Functor.BASIC


module Applicative = struct
  module type BASIC =
    Endofunctors_intf.Applicative.BASIC

  module type AUGMENTED =
    Endofunctors_intf.Applicative.AUGMENTED

  module Make (A : BASIC)
         : AUGMENTED with type 'a t = 'a A.t = struct
    include Functor.Make (A)
    type 'a t = 'a A.t
    let ( and+ ) = A.product
    let apply af ax = A.map
                        (fun (f, x) -> f x)
                        (A.product af ax)
    let ( <*> ) = apply
  end

end
module type APPLICATIVE = Applicative.BASIC

module Monad = struct
  module type BASIC =
    Endofunctors_intf.Monad.BASIC

  module type AUGMENTED =
    Endofunctors_intf.Monad.AUGMENTED

  module Make (M : BASIC)
         : AUGMENTED with type 'a t = 'a M.t
    = struct
    let pure = M.pure
    let bind = M.bind
    let ( >>= ) = bind
    let ( let* ) = bind
    let ( >=> ) mf mg x = mf x >>= mg
    let ( <=< ) mf mg x = mg x >>= mf
    let ( >> ) mx my = mx >>= fun _ -> my
    let join mx = let* x = mx in x

    module I = struct
      type 'a t = 'a M.t
      let map f mx = let* x = mx in
                     pure (f x)
      let product ax ay = let* x = ax in
                          let* y = ay in
                          pure (x,y)
    end
    include Applicative.Make (I)
  end
end
module type MONAD = Monad.BASIC


module State = struct
  module type PURESTATE = sig type t end

  module type BASIC =
    Endofunctors_intf.State.BASIC

  module type MONAD =
    Endofunctors_intf.Monad.AUGMENTED

  module Make (S : PURESTATE) : sig
    type ('s, 'a) t
    include MONAD with type 'a t := (S.t, 'a) t
    include BASIC with type ('s, 'a) t := ('s, 'a) t
  end = struct

    type ('s, 'a) t = 's -> 'a * 's
    module StateMonad = struct
      type 'a t = S.t -> 'a * S.t
      let pure x state = (x, state)
      let bind mx k state1 =
        let ( result1, state2 ) = mx state1 in
        k result1 @@ state2
    end

    let put state _ = ((), state)
    let get state = (state, state)
    
    let eval mx state = mx state |> fst
    let exec mx state = mx state |> snd
    let run mx state = mx state

    module S = Monad.Make (StateMonad)

    let pure = S.pure
    let bind = S.bind
    let map = S.map
    let ( let+ ) = S.( let+ )
    let ( >>| ) = S.( >>| )
    let ( <&> ) = S.( <&> )
    let ( >|= ) = S.( >|= )
    let ( <$> ) = S.( <$> )
    let ( and+ ) = S.( and+ )
    let ( <*> ) = S.( <*> )
    let apply = S.apply
    let ( >>= ) = S.( >>= )
    let ( let* ) = S.( let* )
    let ( >=> ) = S.( >=> )
    let ( <=< ) = S.( <=< )
    let ( >> ) = S.( >> )
    let join = S.join
  end
end
