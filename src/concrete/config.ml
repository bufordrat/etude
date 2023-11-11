let read_paths filepaths =
  let open Option in
  asum (List.map (catch Prelude.readfile) filepaths)

let paths_fallback fallback filepaths =
  Option.default fallback (read_paths filepaths)
