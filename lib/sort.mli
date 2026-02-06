type unranked = Partition | LeaveInPlace

val list_to_ranking :
  ?unranked:unranked -> 'a list -> 'a -> 'a -> int

val list_to_sort :
  ?unranked:unranked -> 'a list -> 'a list -> 'a list
