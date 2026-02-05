let list_to_ranking lst x y =
  let idxes = List.(1 -- length lst) in
  let table = Prelude.(zip lst idxes) in
  let lookup = 
    let open Option in
    let* rank1 = List.assoc_opt x table in
    let* rank2 = List.assoc_opt y table in
    Some (Int.compare rank1 rank2)
  in
  match lookup with
  | None -> 0
  | Some i -> i
