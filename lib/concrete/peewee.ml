let sayings =
  [ "Your wish is granted.  Long live Jambi.";
    "Why don't you take a picture?  It'll last longer.";
    "Mecca lecca hi, mecca hiney ho.";
    "You don't wanna get mixed up with a guy like me.  I'm \
     a loner, Dottie.  A rebel.";
    "I know you are, but what am I?"
  ]

let random () =
  let () = Random.self_init () in
  let idx =
    Random.full_int max_int mod List.length sayings
  in
  List.nth sayings idx
