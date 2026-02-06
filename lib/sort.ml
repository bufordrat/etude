let get_idx key =
  let rec get_idx' n key = function
    | [] -> None
    | x :: xs ->
      if key = x then Some n else get_idx' (n + 1) key xs
  in
  get_idx' 0 key

type unranked = Partition | LeaveInPlace

let list_to_ranking_partition list x y =
  let rank1_opt = get_idx x list in
  let rank2_opt = get_idx y list in
  match (rank1_opt, rank2_opt) with
  | Some _, None -> -1
  | None, Some _ -> 1
  | None, None -> 0
  | Some x, Some y -> Int.compare x y

let list_to_ranking_leave list x y =
  let open Option in
  let ranks =
    let+ rank1 = get_idx x list
    and+ rank2 = get_idx y list in
    (rank1, rank2)
  in
  match ranks with
  | Some (r1, r2) -> Stdlib.compare r1 r2
  | None -> 0

let list_to_ranking ?(unranked = Partition) list x y =
  match unranked with
  | Partition -> list_to_ranking_partition list x y
  | LeaveInPlace -> list_to_ranking_leave list x y

let list_to_sort ?(unranked = Partition) rankings lst =
  let comp x y = list_to_ranking ~unranked rankings x y in
  List.stable_sort comp lst
