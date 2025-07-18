let alternatives lookup lst =
  let open Option in
  asum (List.map lookup lst)

let get_config_path ?(squiggle = true) paths =
  let check_exists path =
    if Sys.file_exists path then Some path else None
  in
  let adjusted_paths =
    if squiggle
    then List.map Prelude.File.squiggle paths
    else paths
  in
  alternatives check_exists adjusted_paths
